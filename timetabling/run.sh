PARAM=$1

rm conjure-output/*solution*
conjure solve timetabling.essence data/$PARAM.json --output-format=json
cat conjure-output/model000001-$PARAM-solution000001.solution.json | jq
