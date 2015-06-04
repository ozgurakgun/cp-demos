"use strict";

importScripts("lodash.js");


var maxShapeDist = 8;

function rotateAndCleanShapes(shapesIn, coloursIn) {
	// First, rotate shapes
	function rot90(a,x,y) {
		return a[a.length-1-y][x];
	}
	
	var shapeList = [];
	var colourList = [];
	var shapeOld;
	var shapeNew;
	for(let i = 0; i < shapesIn.length; ++i) {
		if(_.some(shapesIn[i], (x) => _.some(x))) { // filter empty shapes
			shapeOld = shapesIn[i];
			shapeList.push(shapeOld);
			colourList.push(coloursIn[i]);
			for(let j = 0; j < 3; ++j)
			{
				shapeNew = _.map(_.range(shapeOld[0].length), (x) => _.map(_.range(shapeOld.length), (y) => rot90(shapeOld, x, y)));
				shapeList.push(shapeNew);
				colourList.push(coloursIn[i]);
				shapeOld = shapeNew;
			}
		}
	}
	
	for(let i = 0; i < shapeList.length; ++i)
	{
		let s = shapeList[i];
		while(!(_.some(_.first(s))))
			s = _.drop(s);
		while(!(_.some(_.last(s))))
			s = _.dropRight(s)
		while(!(_.some(_.map(s, (x) => _.first(x)))))
			s = _.map(s, _.drop);
		while(!(_.some(_.map(s, (x) => _.last(x)))))
			s = _.map(s, _.dropRight);
		shapeList[i] = s;
	}

	let outlist = [];
	let outcolourlist = [];

	for(let i = 0; i < shapeList.length; ++i) {
		let found = false;
		for(let j = 0; j < outlist.length && found === false; ++j) {
			if(_.isEqual(shapeList[i], outlist[j])) {
				found = true;
			}
		}
		
		if(!found) {
			outlist.push(shapeList[i]);
			outcolourlist.push(colourList[i]);
		}
	}
	
	maxShapeDist = _.max(_.flattenDeep([_.map(outlist, (x) => x.length),_.map(outlist, (x)=>x[0].length)]));
	
	return { pieces: outlist, pieceColours : outcolourlist };
}


let local_log = function() {}
//let local_log = (...arg) => console.log(...arg)

// Utility

var cartesian = function cartesian() {
	return _.reduce(arguments, function (a, b) {
		return _.flatten(_.map(a, function (x) {
			return _.map(b, function (y) {
				return x.concat([y]);
			});
		}));
	}, [[]]);
};

var deref = function deref(array, x) {
	if (x.length == 1) return array[x[0]];

	if (x.length == 2) {
		if (array[x[0]] === undefined) {
			return array[x[0]];
		} else {
			return array[x[0]][x[1]];
		}
	}
	throw new Error("Bad deref");
};

var addToGrid = function addToGrid(grid, shape, offset, shapeid) {
	var result = _.cloneDeep(grid);
	var i, j;
	for (i = 0; i < shape.length; ++i) {
		for (j = 0; j < shape[i].length; ++j) {
			if(shape[i][j] !== false) {
				local_log(result + "");
				if (deref(result, [i + offset[0], j + offset[1]]) !== false) {
					return false;
				} else {
					result[i + offset[0]][j + offset[1]] = shapeid;
				}
			}
		}
	}
	return result;
};

var removeFromGrid = function removeFromGrid(grid, shape, offset, shapeid) {
	var result = _.cloneDeep(grid);
	var i, j;
	for (i = 0; i < shape.length; ++i) {
		for (j = 0; j < shape[i].length; ++j) {
			if(shape[i][j] !== false) {
				if(deref(result, [i + offset[0], j + offset[1]]) !== shapeid) {
					console.log("Inconsistency!")
					undefined[undefined]=undefined;
				}
				result[i+offset[0]][j+offset[1]] = false;
			}
		}
	}
	return result;
};

var packShapes = function packShapes(grid, piece, offset, shapeid) {
	local_log("packShapes");
	var result = _.cloneDeep(grid);
	var placed = [];
	var i;
	var retgrid;
	//local_log(shape);
	//local_log(grid);
	//local_log("Trying...", _.range(0-shape.length, grid.length+shape.length) , "!" ,
	//					     0-shape[0].length  , grid[0].length+shape[0].length);

	//var places = cartesian(_.range(0 - shape.length, grid.length + shape.length), _.range(0 - shape[0].length, grid[0].length + shape[0].length));

	//places = _.shuffle(places);
	//local_log(places);
	//local_log(shape);
	//local_log(grid);
	//local_log("Trying: ", places[i]);
	retgrid = addToGrid(grid, piece, offset, shapeid);
	if (retgrid !== false) {
		local_log("Success!");
		return retgrid;
	}
return false;
};

// tries to move shapes around to make more space
var shuffleGrid = function(pkg) {
	console.log("Try shuffle");
	let moved = true;
	while(moved)
	{
		moved = false;
		for(let i = 0; i < pkg.shapecords.length; ++i)
		{
			let shape = pkg.shapes[pkg.shapeids[i]];
			let coord = pkg.shapecords[i];
			
			let gridcpy = removeFromGrid(pkg.grid, shape, coord, i+1);
			for(let j = coord[0] - 1; j >= -maxShapeDist; --j)
			{
				let moved = addToGrid(gridcpy, shape, [j, coord[1]], i+1);
				if(moved !== false)
				{
					pkg.grid = moved;
					pkg.shapecords[i] = [j, coord[1]];
					moved = true;
				}
			}
			
			coord = pkg.shapecords[i];
			
			for(let j = coord[1] - 1; j >= -maxShapeDist; --j)
			{
				let moved = addToGrid(gridcpy, shape, [coord[0], j], i+1);
				if(moved !== false)
				{
					pkg.grid = moved;
					pkg.shapecords[i] = [coord[0], j];
					moved = true;
				}
			}
			
		}
		
	}
	return pkg;
}


var packLotsShape = function packLotsShape(obj) {
	var retgrid;
	var shapecords = obj.shapecords;
	var match = true;
	var shapeids = obj.shapeids;
	var grid = obj.grid;
	var shapelist = obj.shapes;
	
	while (match) {
		match = false;
		
		var places = cartesian(_.range(0 - maxShapeDist, grid.length + maxShapeDist),
							   _.range(0 - maxShapeDist, grid[0].length + maxShapeDist),
							   _.range(shapelist.length)
							  );
		
		places = _.shuffle(places);
		
		for (let i = 0; i < places.length; ++i) {
			retgrid = packShapes(grid, shapelist[places[i][2]], [places[i][0], places[i][1]], shapeids.length + 1);
			if (retgrid) {
				shapecords.push([places[i][0], places[i][1]]);
				grid = retgrid;
				match = false;
				shapeids.push(places[i][2]);
			}
		}
	}
	return { shapes: shapelist, grid: grid, shapecords: shapecords, shapeids : shapeids };
};

var packageSolution = function(p, obj) {
	return _.map(_.range(p.shapeids.length), (i) => ({x: p.shapecords[i][0], y: p.shapecords[i][1],
													  shape: obj.pieces[p.shapeids[i]],
													  colour: obj.pieceColours[p.shapeids[i]]}));
}

var makeEmptyGrid = function(obj, rotated) {
	var blankgrid = _.times(obj.packingDim[0],() => _.times(obj.packingDim[1], () => false));
	return { shapes: rotated.pieces, grid: blankgrid, shapecords: [], shapeids: [] };
}

onmessage = function(message)
{
	let obj = message.data;
	local_log("begin");

	let rotated = rotateAndCleanShapes(obj.pieces, obj.pieceColours);
	
	console.log(obj, rotated);
	
	//var blankgrid = makeEmptyGrid(obj, rotated);
	//var blankgrid = _.times(obj.packingDim[0],() => _.times(obj.packingDim[1], () => false));

	let bestresult = 0;
	let bestgrid = undefined;
	for(let i = 0; i < 10000; i++) {
		let blankgrid = makeEmptyGrid(obj, rotated);
		let p = packLotsShape(shuffleGrid(packLotsShape(blankgrid)));
		//let p = packLotsShape(blankgrid);
		let count = _.sum(_.map(p.grid, (x) => _.sum(_.map(x,(y)=>y!==false))));
		console.log(count);
		if(count > bestresult) {
			bestresult = count;
			bestgrid = p;
			postMessage({newPacking: [packageSolution(p, rotated)]});
			//undefined[undefined]=undefined
		}
	}
	//postMessage({ newPacking : _.times(20, () => { let p = packLotsShape(blankgrid, rotated.pieces); return packageSolution(p,rotated)} )});
}
