rm -f conjure-output/*solution*
conjure solve nurse-rostering.essence data/sample.json --output-format=json --line-width 30
cat conjure-output/model000001-sample-solution000001.solution.json
