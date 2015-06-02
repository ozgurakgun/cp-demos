"use strict";

var VarContainer = function VarContainer(solver) {
	this.solver = solver;
	this.varcount = 0;
	this.domains = [];
};

VarContainer.prototype.newVar = function (domain) {
	var v = this.varcount;
	this.domains[v] = domain;
	this.varcount++;
	return v;
};

var Solver = function Solver() {
	this.constraints = [];
	this.vars = new VarContainer(this);
	// sanity checker
	this.isSolver = true;
};

Solver.prototype.newVar = function (domain) {
	return this.vars.newVar(domain);
};

Solver.prototype.addConstraint = function (constraint) {
	this.constraints.push(constraint);
	constraint.addSolver(this);
};

var ConstraintBase = function ConstraintBase() {};

ConstraintBase.prototype.addSolver = function (solver) {
	assert(solver.isSolver);
	this.solver = solver;
};

ConstraintBase.prototype.getVars = function () {
	return this.vars;
};

var BinaryTable = function BinaryTable(vars, tuples) {
	var xvals, yvals;
	this.vars = vars;
	this.tuples = tuples;
	xvals = _.uniq(_.map(this.tuples, function (t) {
		return t[0];
	}));
	yvals = _.uniq(_.map(this.tuples, function (t) {
		return t[1];
	}));
	this.xtups = _.forEach(xvals, function (x) {
		[x, _.map(_.filter(tuples, function (tup) {
			return tup[0] == x;
		}), function (p) {
			return p[1];
		})];
	});
};

BinaryTable.prototype = Object.create(ConstraintBase.prototype);

BinaryTable.prototype.propagate = function () {
	_.filter(this.vars[0].domain(), nonEmptyIntersection());
};

var bintable = new BinaryTable(["a", "b"], [[1, 2], [1, 3], [1, 4], [2, 3], [2, 5]]);

