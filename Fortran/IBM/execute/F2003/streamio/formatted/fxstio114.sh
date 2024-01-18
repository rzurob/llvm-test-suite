#!/usr/bin/ksh

cp $TR_SRC/check_array.inc .
cp $TR_SRC/check_interface.inc .

echo "$KILL xlf95_r $TR_SRC/fxstio114.f $OPTIONS -qsmp=omp -o fxstio114"
$KILL xlf95_r $TR_SRC/fxstio114.f $OPTIONS -qsmp=omp -o fxstio114

rc=$?
if [ $rc -ne 0 ]; then
   echo "Compilation Failed!"
   exit $rc
fi

echo "$KILL fxstio114"
$KILL fxstio114

rc=$?
if [ $rc -ne 0 ]; then
   echo "Execution failed!"
   exit $rc
fi

diff fxstio114.dat $TR_SRC/fxstio114.vf
if [ $? -ne 0 ]; then
   echo "Verification Failed!"
   exit 13
fi

if [[ $TRUN_SAVE != "YES" && $TRUN_SAVE != "yes" ]]; then
   rm -f check_array.inc check_interface.inc
   rm -f fxstio114.o fxstio114 fxstio114.dat
fi
