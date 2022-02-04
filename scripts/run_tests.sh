#!/bin/bash
export ASDAR="https://github.com/rsbivand/sf_asdar2ed/raw/"
#"https://asdar-book.org"
#https://github.com/rsbivand/asdar-book/raw/master/dump_from_bitbucket_repo/"
#http://www.asdar-book.org/"
#export ASDAR_DOWNLOAD="${ASDAR}datasets"
export ASDAR_BUNDLES="${ASDAR}/main"
export R_LIBS="/home/rsb/lib/r_libs"
export R_CMD="/home/rsb/topics/R/R412-share/bin/R"
export RUN_IN="/home/rsb/proj/other/book/evolution_tests"
export MESSAGES="/home/rsb/proj/other/book/evolution_tests/messages"
export LANG=C
export LC_ALL=C

cd $RUN_IN

if test -d ${MESSAGES}
  then rm ${MESSAGES}/*
else
  mkdir ${MESSAGES}
fi

CHAPTERS="hello cm vis die cm2 std sppa geos lat dismap"
STATUS="0 1 2"

for ch in $CHAPTERS; do
  if test -d ${ch}
    then cd ${ch}
  else
    mkdir ${ch}
    cd ${ch}
  fi
  wget -N "${ASDAR_BUNDLES}/${ch}_bundle.zip"
  unzip -o "${ch}_bundle.zip"
  for st in $STATUS; do
    chst="${ch}_${st}"
    echo "Sys.setenv(LC_COLLATE = \"C\", LANGUAGE = \"en\")" > scrpt
    echo "options(\"sp_evolution_status\"=${st}L)" >> scrpt
    echo "source(\"${ch}_mod.R\", echo=TRUE)" >> scrpt
    $R_CMD --vanilla < scrpt 2>&1 > ${chst}.log
    if test $? -ne 0
      then 
        cat >  ${MESSAGES}/${chst} << _EOCONF
evolution_tests: $chst evolution_test failure
_EOCONF
        echo "ASDAR $chst evolution_test failure"
    fi
    echo $LANG >> ${chst}.log
    diff ${chst}.log ${chch}.log.save > ${chst}.diffs
    if test -s ${chst}.diffs
    then
      cat ${chst}.diffs > ${MESSAGES}/${ch}_diff_2ed_failure
      echo "ASDAR $chst diff evolution_test failure"
    fi
  done
  cd ..
done
if test `ls -1 "${MESSAGES}" | wc -l` -ne 0
  then
  cat > remotes << _EOCONF
cd tmp/evolution_tests/messages
rm *
lcd ${MESSAGES}
put *
bye
_EOCONF
  sftp -b remotes "158.37.107.21"
  if test $? -ne 0
    then echo "Failure running remotes"
  fi
fi
