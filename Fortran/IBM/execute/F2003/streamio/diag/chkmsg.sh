#!/bin/ksh
$COMPILER -o $1 $TR_SRC/$1.f $OPTIONS

if [ $? -ne 0 ]
then
   echo "Compilation Failed!"
   rm -f $1
   exit 1
fi

# We have to redirect STDIN and STDOUT to some files, otherwise it will
# be by default redirected to /dev/null by the Load_levelor. This causes
# run-time error when the testcase trys to open /dev/null (B.C. -defect 249979)
touch stdin.txt
$1 < stdin.txt > stdout.txt 2> $1.out

diff $TR_SRC/$1.vf $1.out
rc=$?
if [ $rc -ne 0 ]
then
   echo "Verification Failed!"
   exit $rc
else
   rm -f $1.out $1
fi
