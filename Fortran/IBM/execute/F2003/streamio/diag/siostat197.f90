!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/chkmsg.sh siostat197
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Stream Access I/O
!*
!*  PROGRAMMER                 : Rafik Zurob
!*  DATE                       : April 2003
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : POS= on non-seekable unit
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       integer iostat
       open(11, file='/dev/null', access="stream")
       write(11, pos=3, iostat=iostat) 10
       if (iostat /= 197) error stop 1

       write(11, pos=3) 10
       end

