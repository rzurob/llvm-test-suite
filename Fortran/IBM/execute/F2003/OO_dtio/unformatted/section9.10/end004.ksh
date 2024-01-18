#! /bin/ksh

/.../torolab.ibm.com/fs/projects/vabld/run/xlf/dev/aix/latest/bin/xlf90 -o ./end004  end004.f  -tchb -B/home/xlftest/robertma/localBuild/ -L/home/xlftest/robertma/OO_dtio -L/xlftest/lib -lxlf90 -lxlfrtezz4 -qdebug=ooall -qfree=f90 -qnolm -qfree=f90

if [[ $? -ne 0 ]]; then
   echo "Compilation failed!"
   exit -1
fi

./end004  < /dev/null > end004.out

diff end004.out end004.vf

if [[ $? -ne 0 ]]; then
   echo "FAILED"
   rm end004.1 end004.out end004
   exit -1
fi

rm end004.1 end004.out end004
exit 0