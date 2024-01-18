!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/chkmsg.sh siostat194
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
!*  DESCRIPTION                : Backspace on unformatted stream
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       integer iostat, i, z

       open(11, access='sequential', status='replace', form='formatted')
       write(11, fmt='(A3,/,A3)') 'abc', 'def'
       close(11, status='keep')

       open(11, access='stream', status='old', form='unformatted', position='append')
       backspace(11, iostat=iostat)
       if (iostat /= 194) error stop 1

       backspace(11)
       close(11, status='delete')
       end
