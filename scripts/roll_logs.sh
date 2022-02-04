#! /bin/bash
export RUN_IN="/home/rsb/proj/other/book/evolution_tests"

cd $RUN_IN

CHAPTERS="hello cm vis die cm2 std sppa geos lat dismap"
STATUS="0 1 2"

for ch in $CHAPTERS; do
  cd ${ch}
  for st in $STATUS; do
    chst="${ch}_${st}"
    cp ${chst}.log ${chst}.log.save
    echo "rolled ${chst}.log"
  done
  cd ..
done
