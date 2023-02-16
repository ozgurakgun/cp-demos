rm conjure-output/*solution*
conjure solve task-allocation.essence data/basic.json --output-format=json
cat conjure-output/model000001-basic-solution000001.solution.json | jq
