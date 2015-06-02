"use strict";



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
		console.log("In", s);
		while(!(_.some(_.first(s))))
			s = _.drop(s);
		while(!(_.some(_.last(s))))
			s = _.dropRight(s)
		while(!(_.some(_.map(s, (x) => _.first(x)))))
			s = _.map(s, _.drop);
		while(!(_.some(_.map(s, (x) => _.last(x)))))
			s = _.map(s, _.dropRight);
		console.log("Out", s);
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

var addToGrid = function addToGrid(grid, shape, xoffset, yoffset, shapeid) {
	var result = _.cloneDeep(grid);
	var i, j;
	local_log(grid + "");
	local_log(result + "");
	for (i = 0; i < shape.length; ++i) {
		for (j = 0; j < shape[i].length; ++j) {
			if(shape[i][j] !== false) {
				local_log(result + "");
				local_log("Deep check: ", [i + xoffset, j + yoffset], deref(result, [i + xoffset, j + yoffset]));
				if (deref(result, [i + xoffset, j + yoffset]) !== false) {
					return false;
				} else {
					result[i + xoffset][j + yoffset] = shapeid;
				}
			}
		}
	}
	return result;
};

var packShapes = function packShapes(grid, shape, shapeid) {
	local_log("packShapes");
	var result = _.cloneDeep(grid);
	var placed = [];
	var i;
	var retgrid;
	//local_log(shape);
	//local_log(grid);
	//local_log("Trying...", _.range(0-shape.length, grid.length+shape.length) , "!" ,
	//					     0-shape[0].length  , grid[0].length+shape[0].length);

	var places = cartesian(_.range(0 - shape.length, grid.length + shape.length), _.range(0 - shape[0].length, grid[0].length + shape[0].length));

	places = _.shuffle(places);
	//local_log(places);
	//local_log(shape);
	//local_log(grid);
	for (i = 0; i < _.size(places); ++i) {
		local_log("Trying: ", places[i]);
		retgrid = addToGrid(grid, shape, places[i][0], places[i][1], shapeid);
		if (retgrid !== false) {
			local_log("Success!");
			return { grid: retgrid, location: places[i] };
		}
	}
	return false;
};

var packLotsShape = function packLotsShape(grid, shapelist) {
	var retgrid;
	var shapecords = [];
	var match = true;
	var shapeids = [];
	while (match) {
		match = false;
		for (let i = 0; i < shapelist.length; ++i) {
			retgrid = packShapes(grid, shapelist[i], shapeids.length + 1);
			if (retgrid) {
				shapecords.push(retgrid.location);
				grid = retgrid.grid;
				match = true;
				shapeids.push(i);
			}
		}
		local_log(shapecords.length);
	}
	return { shapes: shapelist, grid: grid, shapecords: shapecords, shapeids : shapeids };
};

var packageSolution = function(p, obj) {
	return _.map(_.range(p.shapeids.length), (i) => ({x: p.shapecords[i][0], y: p.shapecords[i][1],
													  shape: obj.pieces[p.shapeids[i]],
													  colour: obj.pieceColours[p.shapeids[i]]}));
}

var doLocalSearch = function(obj)
{
	local_log("begin");

	let rotated = rotateAndCleanShapes(obj.pieces, obj.pieceColours);
	
	console.log(obj)
	var blankgrid = _.times(obj.packingDim[0],() => _.times(obj.packingDim[1], () => false));

	return { newPacking : _.times(20, () => { let p = packLotsShape(blankgrid, rotated.pieces); return packageSolution(p,rotated)} )}
}
