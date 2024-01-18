!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrf011.f
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
!*  DESCRIPTION                : Test the association of a function result with
!*                               a character dummy argument with VALUE attribute.
!*                               Test character of length 1.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1)
           character(1), value :: c1
         end subroutine
         function f1()
           character(1) f1
         end function
      end interface

      character(1) :: c

      call s1(f1())

      c = f1()

      call s1(c)

      if ( c .ne. 'X' ) error stop 3

      end

      subroutine s1(c1)
        character(1), value :: c1
        if ( c1 .ne. 'X' ) error stop 1
        if ( c1//c1 .ne. 'XX' ) error stop 2
        c1 = 'a'
      end subroutine

      function f1()
        character(1) f1
        f1 = 'X'
      end function
