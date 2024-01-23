#!/bin/sh
### vfloop -- loop over multiple verification files corresponding to one
###   set of source files
#
# $Id$  %Z% %H% %I%
# $Source$  %Z% %P%

### Hardcoded constants
myname=`basename $0`

usage()
{
    echo >&2  "Usage-1 (via trun): PARSER_TARGET=<tc pathname>.f $myname"
    echo >&2  "Usage-2 (manual): $myname <tc pathname>.vf ..."
    echo >&2  '  env var $COMPILER is alw required; $OPTIONS may also be used.'
    exit 1
}


listf=./$myname.$$.vflist
errf=./$myname.$$.errsig
echo "listf=$listf, errf=$errf"
cleanup()
{
    rc=$1
    flg=$2

    /bin/rm -f $listf $errf

    if [ "$flg" -eq 1 ]
    then
        echo "${myname}: caught signal ... terminating"
    fi

    exit $rc
}

exitStatus=1
trap "cleanup 127 1"  1 2 3 15

make_silent=-s  # default: -s when not debugging
case $vfloop_dbg in ?*)
    make_silent=
esac

test -f /etc/issue  && linuxDist=`whichLx`

case `isAIX`.$linuxDist in
    0.*) makecommand=gnumake ;;  # AIX
    1.1) makecommand=make ;;     # RedHat Enterprise Linux
    1.0) makecommand=make ;;     # SuSE Linux
    2.*) makecommand=make ;;     # MacOSX

       # facilitate debugging: user will eventually get an
       # error like `file unknownOS-<testcase>.vf not found'.
    *) echo unknownOS ;;
esac


### Functions
compute_sname()
{
  ## Usage: compute_sname <pathname with suffix> <suffix>
  ##   $TR_SRC must pre-exist.
  ## sname: common Source name, eg `/tstdev/dummy-sub'.
  ## cfname: ConFiguration script name eg `/tstdev/dummy-sub.mk'.
  ## oname: Object basename eg `dummy-sub'.
  ## genFiles: basenames of likely generated files eg `dummy-sub.lst'.
  #
  # dirname is saved in $TR_SRC, remove it to simplify patterns, then
  # add it back after:
  #
  oname=`echo $1 |sed '
    s#.*/##

    /-[0-9]/{ s/-[0-9].*//; q;}
    /\.-/{    s/\.-.*//;    q;}
    s/'$2'//
  '`

  sname=$TR_SRC/$oname
  genFiles="$oname.o $oname.mod $oname.lst  $oname"
  osname=`$TR_SRC/osname.sh`  # set operating system prefix.  Eg `-aix'

  ## If -cf not explicitly specified on command line,
  ##  search for scripts related to testcase.  First one wins.
  case $cfname in '')
    for j in .sh .mk .F .f; do
      test -f $sname$j  && { cfname=$sname$j; break;}
    done
  esac 
}


### Parse arguments; gen file containing list of verification files
case $COMPILER.$OPTIONS in .)
    usage
esac

case $1 in -cf)
    cfname=$2; shift; shift;
esac
echo "PARSER_TARGET=$PARSER_TARGET"
case $# in
    0)  # trun invocation: no args; env var contains .f filename
      case $PARSER_TARGET in '')
        echo "this is bad"
        usage
      esac

      compute_sname $PARSER_TARGET '\.f'

      # Use a pipeline to prevent limits on max number of files.
      # Unfortunately this requires us to strip dirname, then add it back:
      #
      pat=`basename $PARSER_TARGET .f`

      ls $TR_SRC  |sed -n  >$listf  "
        s#^$pat-$osname-[0-9].*\.vf\$#$TR_SRC/&#p
        s#^$pat-$osname\.-.*\.vf\$#$TR_SRC/&#p
        s#^$pat-$osname\.vf\$#$TR_SRC/&#p
      "

      test -s $listf  || {
        echo >&2  "$myname: no verification files found.  Aborting."
        echo >&2  "  PARSER_TARGET=\"$PARSER_TARGET\"  args=\"$*\"."
        exit 1
      }
      ;;

    *)  # manual invocation: args contain list of verification files
      for j do
        case $j in
          *.vf) echo "$j" ;;
          *) usage ;;
        esac  >$listf
      done

      case $TR_SRC in '')
        TR_SRC=`dirname $1`
      esac

      compute_sname $1 '\.vf'
      ;;
esac


#
#  Ensure that CC is set.  If it isn't set, initialize it to "xlc".
#
if [ -z "$CC" ]
then
    compiler=`expr "$COMPILER" : '.*/\(xlf.*\)$'`
    compType=`expr "$compiler" : 'xlf[^_]*\(_r\)'`
    if [ -n "$compType" ]
    then
        CC="xlc_r"
    else
        CC="xlc"
    fi
    export CC
fi


${vfloop_dbg:+echo} : "$myname init:\n" \
    " sname=$sname\n  oname=$oname\n  cfname=$cfname\n" \
    " genFiles=$genFiles\n" \
    " COMPILER=$COMPILER\n  OPTIONS=$OPTIONS\n" \
    " PARSER_TARGET=$PARSER_TARGET\n  TR_SRC=$TR_SRC\n  0=$0  *=$*\n"

SILENT_BUILD=
PDF_OPT=`expr "$OPTIONS" : '.*\(-qpdf[12]\)'`
export PDF_OPT SILENT_BUILD


### Main
myrv=0  # My Return Value

echo "cfname is $cfname"
cat $listf  |while read vfname; do
    ## vfname: VeriFication filename, eg `/tstdev/dummy-sub-13.-qfixed.vf'.
    set +x  # in case we short-circuited somehow.

    ## tcname: Test Case name, eg `dummy-sub-13.-qfixed'.
    #
    tcname=`basename $vfname .vf`
    case $tcname in '')
      echo >&2  "$myname: cannot parse verification name \"$vfname\"." \
        " Aborting."
    esac

    ## nameOpt: Options embeded directly in filename, eg `-qfixed -qintmsg'.
    # Algorithm: if dot present, discard any preceding text and then insert
    # spaces before each dash.
    #
    nameOpt=`echo $tcname |sed -n '/\./{s/^.*\.//; s/-/ -/pg;}'`

    ${vfloop_dbg:+echo} : \
      "\n  vfname=$vfname\n  tcname=$tcname\n  nameOpt=$nameOpt\n"

    (  # isolate any imported iteration variables in this sub-shell.

      /bin/rm -f $tcname.out $tcname.skip

      ## Compute $fF, $cF, headerMsg, and $skip depending on
      ## verification file variant number, $nameOpt, $COMPILER, and $OPTIONS.
      # (fF = Fortran Flags, cF = C Flags)
      test -f $sname.tcopt  && {
        ${vfloop_dbg:+set -x}
          tcoptFound=1  # force testcase variant header to be output
          eval `nameOpt=$nameOpt  sh $sname.tcopt $tcname`
        set +x
      }

      ## Some testcase variants must not be run in conjunction with
      ## some compilation options which the user might supply.
      ## $skip allows .tcopt to tell us to skip a numeric variant.
      case $skip in 1)
        echo skipping $tcname
        touch $tcname.skip
        exit 0
      esac

      ## Output testcase variant header
      # $nameOpt isn't yet in $fF.
      #
      # $tcoptFound: if a .tcopt file exists, the verification filename
      # is usually numeric so, to facilitate visual inspection,
      # output a header for the null-options variant too:
      #
      echo testcase variant $tcname
      case "$headerMsg$fF$nameOpt$tcoptFound" in ?*)
        echo '*** testcase variant:' \
          "${headerMsg-${fF:-${tcoptFound+'<no options>'}}}$nameOpt\n" \
          >>$tcname.out
      esac

      ## fF+nameOpt
      # to prevent oversights, concatenate options from .tcopt with
      # those embedded in testcase filename.
      #
      fF="$fF$nameOpt"  # $nameOpt, if not empty, contains a leading space

      ## Copy selected options from $fF to $cF
      # Some Fortran compilation options must also be used in the
      # C compilation. c-opt.sh selects relevant Fortran options
      # and maps them to C options, echoing the results.
      #
      cF="$cF `$TR_SRC/c-opt.sh $OPTIONS $fF`"

      ${vfloop_dbg:+set -x}  # till parens-subshell exits

      if [ -n "$PDF_OPT" ]
      then
        PDF_FILE="${tcname}.pdf"
        PDF_NAME="-qipa=pdfname=${PDF_FILE}"
        export PDF_FILE PDF_NAME
      fi

      case $cfname in
        *.[Ff])  # pre-run cleanup, manual compile, run:
          /bin/rm -f $genFiles

          # NB: if -c supplied in $fF or $OPTIONS, then $oname will contain
          # an object instead of an a.out .  Since it won't be executable,
          # that's not a problem.
          $COMPILER $OPTIONS $PDF_NAME $fF $cfname \
                           -o $oname 1>>$tcname.out 2>&1

          # executable may absent for any of these reasons:
          #   * compilation was intended to fail
          #   * compilation failed in error
          #   * compilation used -c
          #
          test -x $oname && file $oname | grep executable && ./$oname >>$tcname.out 2>&1
          ;;

        *.sh)
          fF="$PDF_NAME $fF" cF="$PDF_NAME $cF" \
                   $cfname $sname 1>>$tcname.out 2>&1
          ;;

        *.mk)
          # Unless debugging, use silent make (`-s') to prevent compile
          # options and compiler path from showing up in output where
          # it would cause differences from one run to another.
          #
          # The `tc' target cleans, then builds, and runs.
          # Use same target for all testcases.
          #
          newname=`echo $oname |sed 's/-.*//'`
          if [ -n "$PDF_OPT" ]
          then
            newname=`echo "$oname" | sed -e 's/-.*//' -e 's/$/-f/'`

            PDF_FILE="f_${tcname}.pdf"
            PDF_NAME="-qipa=pdfname=$PDF_FILE"
            export PDF_FILE PDF_NAME
          fi

          $makecommand $make_silent -f $TR_SRC/makefile.include       \
            -f $cfname "fF=$OPTIONS $PDF_NAME $fF" "cF=$PDF_NAME $cF" \
                                             tc-$newname 1>>$tcname.out 2>&1

          if [ -n "$PDF_OPT" ]
          then
            SILENT_BUILD="1> /dev/null 2>&1"
            export SILENT_BUILD

            newname=`echo "$oname" | sed -e 's/-.*//' -e 's/$/-c/'`

            PDF_FILE="c_${tcname}.pdf"
            PDF_NAME="-qipa=pdfname=$PDF_FILE"
            export PDF_FILE PDF_NAME

            $makecommand $make_silent -f $TR_SRC/makefile.include       \
              -f $cfname "fF=$OPTIONS $PDF_NAME $fF" "cF=$PDF_NAME $cF" \
                                               tc-$newname 1>>$tcname.out 2>&1
          fi
          ;;

        *)
          echo >&2  "$myname: error insufficient support files for" \
            "verification file \"$vfname\".  Skipping." ;;
      esac

      if [ "$PDF_OPT" = -qpdf2 -a "$TRUN_SAVE" != yes ]
      then
        rm -f $PDF_FILE
      fi
    )

    test -f $tcname.skip  && { /bin/rm -f $tcname.skip;  continue;}

    ## Compare with verification file
    case $vfloop_dbg.$TCO in
      .) xldiff $vfname $tcname.out ;;  # normal case
      .?*) cat $tcname.out |
        $TCO $vfname $cfname $sname.[Ff] ;;  # ic's Test Case Output debugger
      ?*) cat $tcname.out ;;  # when debugging, just dump saved output
    esac

    ## Report any verification failure
    case $? in 0) :;; *)
      #??# myrv=`expr $myrv + $?`  # in loop subshell, so this datum is lost
      touch $errf
      echo $vfname FAILED
    esac

    ## Final cleanup.
    case $TRUN_SAVE in ''|[Nn]*)
      ${vfloop_dbg:+set -x}

        case $cfname in
          *.[Ff]) ${vfloop_dbg:+echo}  /bin/rm -f $genFiles ;;
          *.sh) :;;  # does its own cleanup
          *.mk)
            newname=`echo $oname |sed 's/-.*//'`
            ${vfloop_dbg:+echo}  $makecommand -s \
              -f $TR_SRC/makefile.include  -f $cfname \
              cl-$newname
            ;;
        esac

        /bin/rm -f $tcname.out

      set +x
    esac
done

#??# exit $myrv
if [ ! -f $errf ]
then
  exitStatus=0
fi

cleanup $exitStatus 0 
