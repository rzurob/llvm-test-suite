!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: poswrite.f
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
!*  DESCRIPTION                : Unformatted stream write.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
       integer pos, size
       character*4 c4
       character*3 c3

       open(unit=11, access="stream", status='scratch')

       ! File storage units need not be written in order.

       write(11, pos=10) "abc"
       write(11, pos=3) "def"
       write(11) "g"           ! no truncation should occur

       inquire(11, pos=pos, size=size)
       if (pos /= 7) error stop 1
       if (size /= 12) error stop 2

       read(11, pos=3) c4
       if (c4 /= 'defg') error stop 3
       read(11, pos=10) c3
       if (c3 /= 'abc') error stop 4

       ! For unformatted stream output, using the POS= specifier
       ! with an empty output list will extend the terminal point
       ! of the file without actually writing any data.

       write(11, pos=20)
       inquire(11, pos=pos, size=size)
       if (pos /= 20) error stop 5
       if (size /= 12) error stop 6

       close(11)
       end

