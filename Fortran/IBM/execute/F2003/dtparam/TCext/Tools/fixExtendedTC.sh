#!/bin/sh
#
#  To perform appropriate manipulations on the output from the Test Case
#  Extension Tools (created by David and Gaby) to implement the Extended
#  Test Case.
#
#  Usage: $ fixExtendedTC.sh <OriginalTestCasePath>
#
#  Assumptions:
#  o  There is a Local Development Area for the Extended Test Cases (such
#     as "/home/mateer/Development/f289057.F2003TCx/tstdev").
#  o  Extended Test Cases will be implemented into this Local Area using
#     the same relative directory paths as will exist within the Release.
#  o  The tarball created for Test Case Extensions using Gaby's Tool will
#     be located in a similar directory structure in the "xlftest" account:
#
#     "/home/xlftest/common/mateer/Development/f289057.F2003TCx/tstdev/F2003/dtparam/TCext/ace/unit_tests"
#

#
#  Base Values (ie: User Defined -- and changable as required):
#
feature=f289057.F2003TCx
baseHomeDir=/home/mateer/Development
baseXlftestDir=/home/xlftest/common/mateer/Development
originalAuthor='David Forster'


#
#  Constant Values:
#
dtpDir=/tstdev/F2003/dtparam/TCext


#
#  Validate User Input (Part I):
#
PROG_NAME=`basename $0`

if [ $# -ne 1 ]
then
	echo "Usage: $PROG_NAME <TestCasePath>" 1>&2
	exit 1

elif [ -z "$1" ]
then
	echo "Invalid Test Case: ($1)" 1>&2
	exit 2
fi


#
#  Test Case Specific Values:
#
testCase="$1"

dir=`dirname $testCase`
file=`basename $testCase`
base=`basename $testCase .f`


#
#  Validate User Input (Part II):
#
if [ ! -d "$dir" ]
then
	echo "No such directory: ($dir)" 1>&2
	exit 3

elif [ ! -f "$testCase" ]
then
	echo "No such file: ($testCase)" 1>&2
	exit 4

else
	if [ "$base" = "$file" ]
	then
		echo "Fortran Source File required: ($base)" 1>&2
		exit 5
	fi
fi


#
#  Identify the Test List.
#
testList=`/bin/ls -1 *.tl | head -1`

testCaseDir=`echo "$dir" | sed -e 's,^.*/F2003/,,'`
variationsFile="$baseXlftestDir/$feature$dtpDir/$testCaseDir/$base.tar.gz"


#
#  Identify the manipulations required:
#  o Test Case Extension via David's Tool -- David's Tool will dump the
#    Extended Test Case into the Original Directory (relative to the
#    current path options specified in "filenames.txt").
#
#    => a) The .scenario file is adjusted using "fixScenario.pl" and the
#          output file is created in the current directory, all other files
#          are simply moved into the current directory.
#       b) Add the .scenario file to the current directory's .tl file.
#
filePath="$baseHomeDir/$feature$dir"
if [ -f "$filePath/$base"*.f ]
then
	for file in "$filePath/$base"*
	do
		b=`basename $file`
		ext=`echo $file | sed -e 's,^.*\.\([^\.]*\)$,\1,'`
		if [ "$ext" = scenario ]
		then
			echo "Fix .scenario ($b)"
			fixScenario.pl	\
				-l 'F2003/abstracti:s#(F2003/)(abstracti/)#$1dtparam/TCext/$2#'\
				-l '^#FEATURE:s/= (.+) (\+ )d(erived )t(ype )p(arameters)/= D$3T$4P$5 ($2$1)/' \
				-l '^#TEST_TYPE:s/= .+$/= Functional/' \
				-l '^#CREATOR:s/= (.+)$/= Glen Mateer (based on work by $1)/' \
				-l "automated conversion script:s/automated conversion script/$originalAuthor/" \
				"$file" 1> "$b"

			if [ -f "$testList" ]
			then
				echo "$b" >> "$testList"
			fi
		else
			echo "Moving ($b)"
			mv "$file" .
		fi
	done


#
#  o Test Case Extension via Gaby's Tool (Part II -- Select one variation)
#    -- Refer to Part I below to see how we get to this point.
#
#    => a) Select one of the variations.
#       b) Link the .f, .scenario, and .vf (if available) into the current
#          directory.
#       c) Adjust the ".im" for the Extended Test Case in the current
#          directory's .tl file to reference the selected variation.
#       d) Create a fresh tarball in the "Extensions" directory (to include
#          any adjustments required to ensure that one -- or more --
#          variations of the Test Case are correct).
#
elif [ -d "$base" ]
then
	chosenTC=
	while [ ! -f "$chosenTC" ]
	do
		echo
		/bin/ls -1 "$base/"*.f
		echo "Select a Test Case:"
		read chosenTC
	done

	b=`basename $chosenTC .f`
	for file in "$base/$b".*
	do
		dotFExt=`expr "$file" : '[^\.]*\.f\(\..*\)$'`
		if [ -n "$dotFExt" ]
		then
			continue
		fi

		if [ "$file" = "$base/$b.cols" ]
		then
			continue
		fi

		if [ "$file" = "$base/$b.lines" ]
		then
			continue
		fi

		ln "$file" .
	done

	if [ -f "$testList" ]
	then
		sed -e "s,.im $base/$base.tl,$b.scenario," "$testList" > "$testList.NEW"
		mv "$testList.NEW" "$testList"
	fi

	mkdir -p Extensions
	tar cvf - "$base" | gzip > "Extensions/$base.tar.gz"


#
#  o Test Case Extension via Gaby's Tool (Part I -- Test all possible
#    variations) -- with Gaby's Tool it's possible to generate many
#    variations for a given Test Case Extension.  These variations are
#    collected in a tarball (refer to "ext.sh" for details on how this
#    tarball is created).
#
#    => a) Extract the tarball into the Current Directory (creates a
#          directory "<OriginalTestCaseBasename>" in the current directory).
#       b) Add a ".im" for the .tl file from the tarball to the .tl file
#          in the current directory.
#
elif [ -f "$variationsFile" ]
then
	gunzip < "$variationsFile" | tar xvf -

	if [ -f "$testList" ]
	then
		echo ".im $base/$base.tl" 1>> "$testList"
	fi


#
#  No Test Case Extension exists.
#
else
	echo "${PROG_NAME}: no Test Case Extension for this Test Case exists." 1>&2
fi
