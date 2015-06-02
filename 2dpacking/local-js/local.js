"use strict";

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
		alert(grid);
		match = false;
		var i;
		for (i = 0; i < shapelist.length; ++i) {
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
	var cornergrid = _.times(obj.packingDim[0],() => _.times(obj.packingDim[1], () => false));
	
	for(let i = 0; i < p.shapecords.length; ++i) {
		cornergrid[p.shapecords[i][0]][p.shapecords[i][1]] = i+1;
	}
	return { packings : {packing: cornergrid},  bitsPerId : _.map(p.shapeids, (x) => pieces[x]), colourPerId : _.map(p.shapeids, (x) => pieceColours[x]) };
}

var doLocalSearch = function(obj)
{
	local_log("begin");
	let pieceColours = obj.pieceColours;
	let pieces = obj.pieces;
	
	console.log(obj)
	var blankgrid = _.times(obj.packingDim[0],() => _.times(obj.packingDim[1], () => false));
	
	var p = packLotsShape(blankgrid, pieces);
	local_log("end");
	console.log(p);
	
	var cornergrid = _.times(obj.packingDim[0],() => _.times(obj.packingDim[1], () => false));
	
	for(let i = 0; i < p.shapecords.length; ++i) {
		cornergrid[p.shapecords[i][0]][p.shapecords[i][1]] = i+1;
	}
	
	console.log(p.grid);
	console.log(cornergrid);
	console.log("shapes", p.shapeids);
	console.log("pieces", pieces);
	console.log(_.map(p.shapeids, (x) => pieces[x]));
	return { packings : [{packing: cornergrid}],  bitsPerId : _.map(p.shapeids, (x) => pieces[x]), colourPerId : _.map(p.shapeids, (x) => pieceColours[x]) };
}
