#!/bin/sh
### build a pair of source files (one C, one Fortran), then link and run.
#
# sname: common Source name, eg `/tstdev/dummy-sub'.
# oname: Object basename eg `dummy-sub'.

### Parse arguments.
  case $1 in
    '') sname=`expr $0 : .sh` oname=`basename $0 .sh` ;;
    *)  sname=$1              oname=`basename $1`     ; shift ;;  # TRUN
  esac
  genFiles="$oname-c.o  $oname.o $oname.mod $oname.lst  $oname"
  : ${CC:=xlc}  # env var CC takes precedence, else use xlc

### Main.
  trap "/bin/rm -f $genFiles;  exit"  0 1 2 3 15
  set -e ${c_f_pair_dbg:+-x}  # abort on first error; display cmds if debug on

  /bin/rm -f $genFiles
  $CC                $cF $* -c $sname-c.c
  $COMPILER $OPTIONS $fF $* -c $sname.f
  $COMPILER $OPTIONS $fF $*  $oname-c.o $oname.o  -o $oname
  ./$oname
