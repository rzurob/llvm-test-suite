!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: question4a.f
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
!*  DESCRIPTION                : A edit descriptor with output list
!*                               containing \n should be equivalent
!*                               to using the / descriptor.  (i.e.
!*                               a new record will be created.)
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      character*2 :: c, d
      open (11, access='stream', form='formatted', status='scratch')

      write(11, '(A5,T1,A1)', advance='no') 'ab\ncd', 'E'

      rewind(11)

      read(11, '(A2,/,A2)') c, d
      if (c /= 'ab') error stop 1
      if (d /= 'Ed') error stop 2

      rewind(11)
      endfile(11)

      write(11, '(A2,/,A2,T1,A1)', advance='no') 'ab', 'cd', 'E'

      rewind(11)

      read(11, '(A2,/,A2)') d, c
      if (d /= 'ab') error stop 3
      if (c /= 'Ed') error stop 4

      end
