#
# a program that compiles the .f90 source code first for falloc018a1.f
#         then build the executable
#

#!/bin/ksh

rm -f *.o *.mod

SOURCE_CODES="falloc018a1_1.f90 falloc018a1_2.f90 falloc018a1_3.f90"
OBJECTS_NAME=$(echo $SOURCE_CODES | sed 's/\.f90/\.o/g')

for source in $SOURCE_CODES
do
    echo $COMPILER $OPTIONS -c -qsuffix=f=f90 $TR_SRC/$source
    $COMPILER $OPTIONS -c -qsuffix=f=f90 $TR_SRC/$source

    if [[ $? != 0 ]]
    then
        echo "compilation of $source failed"
        exit 1
    fi
done


echo $COMPILER $OPTIONS -c  $TR_SRC/falloc018a1.f
$COMPILER $OPTIONS -c  $TR_SRC/falloc018a1.f

if [[ $? != 0 ]]
then
    echo "compilation of $TR_SRC/falloc018a1.f failed"
    exit 2
fi


## build the executable falloc018a1
echo $COMPILER -o ./falloc018a1 falloc018a1.o $OBJECTS_NAME $OPTIONS
$COMPILER -o ./falloc018a1 falloc018a1.o $OBJECTS_NAME $OPTIONS

if [[ $? != 0 ]]
then
    echo "linking failed"
    exit 3
fi

## execute the program
echo falloc018a1 > falloc018a1.out
./falloc018a1 > falloc018a1.out

if [[ $? != 0 ]]
then
    echo "execution failed"
    exit 4
fi

## verify the results
diff falloc018a1.out $TR_SRC/falloc018a1.vf

if [[ $? != 0 ]]
then
    echo "verification failed"
    exit 5
fi

