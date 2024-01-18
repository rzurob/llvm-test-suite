!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrf008.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 25, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the VALUE
!*                               attribute when used with characters of
!*                               length other than 1. ( Feature 298120 )
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test concatenation of characters with VALUE attribute
!*                               Test character of length 1.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1, c2)
           character(1), value :: c1, c2
         end subroutine
      end interface

      character(1) x, y

      x = 'A'
      y = 'B'

      call s1(x, y)

      if( x .ne. 'A' ) error stop 7
      if( y .ne. 'B' ) error stop 8

      end


      subroutine s1(arg1, arg2)

        character(1), value :: arg1, arg2
        character(2) :: tmp

        if ( (arg1 // arg2) .ne. "AB" ) error stop 1

        tmp = arg2 // arg1

        if ( tmp .ne. "BA" ) error stop 2

        if ( (arg1 // arg1) .ne. "AA" ) error stop 3

        tmp = arg2 // arg2

        if ( tmp .ne. "BB" ) error stop 4

        if ( (arg2 // "q") .ne. "Bq" ) error stop 5

        tmp = "z" // arg1

        if ( tmp .ne. "zA" ) error stop 6

      end subroutine s1

