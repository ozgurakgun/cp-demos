rm conjure-output/*solution*
conjure solve timetabling.essence data/sample.json --output-format=json
cat conjure-output/model000001-sample-solution000001.solution.json | jq
