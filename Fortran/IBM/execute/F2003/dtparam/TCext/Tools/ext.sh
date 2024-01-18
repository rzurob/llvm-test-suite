#!/bin/sh
#
#  To act as a wrapper for Gaby's Test Case Extension Tool that collects
#  the extended Test Case(s) -- along with their supporting files -- into
#  a tarball.
#
#  Usage:  $ ext.sh [-vf] [-utc] <OriginalTestCasePath>
#
#  -vf  Retain Line/Column data for Diagnostic Verification files
#  -utc Identify Extended Test Case as a Unit Test Case in the .scenario file
#
#  Extensions will be performed in a subdirectory of the current directory
#  (this subdirectory will be the basename for the Test Case to be extended).
#  A .tl file will be created in this subdirectory with a list of the
#  variations created.  This subdirectory will be deleted after the tarball
#  has been created.
#
#  Assumptions:
#  o  Uses "tcx.pl" to generate the Extended Test Cases.
#  o  Uses "vf_dtp" (Gaby's Tool to correct .vf Files for Extended Diagnostic
#     Test Cases).
#  o  All support files for a Test Case will have the same basename as the
#     .f file.
#  o  The .scenario file will be adjusted using "fixScenario.pl".
#  o  "tcx.pl","vf_dtp", and "fixScenario.pl" are available via $PATH.
#

#
#  "tcx.pl" Specific Adjustable Values:
#
programmer='Glen Mateer'
primaryFunction='Derived Type Parameters'
secondaryFunction='Abstract Interface'
feature='289057.F2003TCx'
xlfStanza='xlf2003'

#
#  .scenario File Specific Adjustable Edits
#
nameEdit='^#NAME[ 	]*:s#(F2003/)(abstracti/)#$1dtparam/TCext/$2#'
featureEdit='^#FEATURE:s/(Abstract Interface Tests)/'
featureEdit="$featureEdit"'Derived Type Parameters (\+ $1)/'
creatorEdit1='^#CREATOR:s/= (.*)$/= Glen Mateer (derived from work by $1)/'
creatorEdit2='Robert Ma:s/Robert Ma/Alberto Alvarez-Mesquida/'

export FTCX_DTP_KL=k
export FTCX_DTP_LL=l


#
#  FixScenario() - To run "fixScenario.pl" on the given .scenario file.
#  The following edits are applied (as customized above):
#
#  o #NAME -- to correct the Test Case path,
#  o #FEATURE -- to correct the Feature assocated with this Test Case,
#  o #CREATOR (1) -- to indicate the original .scenario file Author,
#  o #CREATOR (2) -- to correct original Author Information (if required).
#
#  The following standard Edits will also be applied:
#
#  o #DATE -- to apply Today's Date
#  o #TEST_TYPE -- to change "sanity" to "Functional"
#
FixScenario( )
{
	original="$1"
	extended="$2"
	tcType="$3"

	baseOrig=`basename $original .scenario`
	baseExt=`basename $extended .scenario`

	dateEdit='^#DATE:s/= \d+$/= '`date '+%Y%m%d'`'/'
	tcNameEdit="$baseOrig:s/$baseOrig/$baseExt/g"
	testTypeEdit="^#TEST_TYPE:s/sanity/$tcType/"

	cr2=
	if [ -n "$creatorEdit2" ]
	then
		cr2="-l \"$creatorEdit2\""
	fi

	fixScenario.pl -l "$nameEdit" -l "$featureEdit" -l "$dateEdit"	\
			-l "$creatorEdit1" -l "$creatorEdit2" -l "$testTypeEdit"\
								-l "$tcNameEdit" "$original" 1> "$extended"

	return
}


#
#  Parse Command Line Options:
#
vfOpt=
unitTestCase=0

while [ true ]
do
	case "$1" in
		-vf)
			vfOpt="$1"
			;;
		-utc)
			unitTestCase=1
			;;
		*)
			break
			;;
	esac

	shift
done


#
#  Basic Test Case Information
#
testCase="$1"
dir=`dirname "$testCase"`
base=`basename "$testCase" .f`

if [ ! -f "$testCase" ]
then
	echo "No such file or directory!" 1>&2
	echo "    Test Case ($testCase)" 1>&2
	exit 1
fi


#
#  Create Test Case Directory
#
mkdir "$base"
cd "$base"


#
#  Generate a list of possible Test Case Extensions ...
#
extended=`tcx.pl -p "$programmer" -1 "$primaryFunction"	\
			-2 "$secondaryFunction" -f "$feature" $vfOpt \
								-s "$xlfStanza" "$testCase"`


#
#  For each Extended Test Case, provide a set of the associated
#  support files.
#
for ext in $extended
do
	echo "Process Support Files for $ext ..."

	newBase=`expr "$ext" : '\([^\.]*\)\.'`
	for file in "$dir/${base}"*
	do
		extType=`expr "$file" : '[^\.]*\.\(.*\)$'`
		newFile="${newBase}.$extType"

		if [ "$extType" = scenario ]
		then
			testCaseType=Functional
			if [ -n "$vfOpt" ]
			then
				testCaseType=Diagnostic
			fi

			if [ "$unitTestCase" -eq 1 ]
			then
				testCaseType='Unit Test'
			fi

			FixScenario "$file" "$newFile" "$testCaseType"
			echo "$newFile" >> "${base}.tl"

		elif [ "$extType" != f -a "$extType" != F ]
		then
			sed -e "s/$base/$newBase/g" "$file" 1> "$newFile"

			if [ "$extType" = vf  -a	\
					-n "$vfOpt" ]
			then
				mv "$newFile" "${newFile}.0"

				vf_dtp "${newFile}.0" "${newBase}.lines"	\
								"${newBase}.cols" "$newFile"

				rm -f "${newFile}.0"
			fi
		fi
	done
done


#
#  Cleanup. ...
#
cd ..

files=`/bin/ls -1 "$base"`
if [ -n "$files" ]
then
	tar cvf - "$base" | gzip > "${base}.tar.gz"
fi

rm -r "$base"
