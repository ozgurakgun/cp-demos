rm -f conjure-output/*solution*

conjure solve nurse-rostering.essence data/$1.json \
    --channelling=no -aai --responses=2 \
    --output-format=json \
    --line-width 30 \
    --solver=kissat \
    --solutions-in-one-file \
    --savilerow-options "-sat-sum-mdd"


# cat conjure-output/model000001-$1.solutions.json
