#! /bin/ksh

/.../torolab.ibm.com/fs/projects/vabld/run/xlf/dev/aix/latest/bin/xlf90 -o ./end004a  end004a.f  -tchb -B/home/xlftest/robertma/localBuild/ -L/home/xlftest/robertma/OO_dtio -L/xlftest/lib -lxlf90 -lxlfrtezz4 -qdebug=ooall -qfree=f90 -qnolm -qfree=f90

if [[ $? -ne 0 ]]; then
   echo "Compilation failed!"
   exit -1
fi

./end004a  < /dev/null > end004a.out

diff end004a.out end004a.vf

if [[ $? -ne 0 ]]; then
   echo "FAILED"
   rm end004a.1 end004a.out end004a
   exit -1
fi

rm end004a.1 end004a.out end004a
exit 0