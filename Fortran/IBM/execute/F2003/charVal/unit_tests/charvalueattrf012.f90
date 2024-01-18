!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 25, 2006
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
!*                               Test character of length 3.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1)
           character(3), value :: c1
         end subroutine
         function f1()
           character(3) f1
         end function
      end interface

      character(3) :: c

      call s1(f1())

      c = f1()

      call s1(c)

      if ( c .ne. 'XyX' ) error stop 3

      end

      subroutine s1(c1)
        character(3), value :: c1
        if ( c1 .ne. 'XyX' ) error stop 1
        if ( c1//c1 .ne. 'XyXXyX' ) error stop 2
        c1 = 'aaa'
      end subroutine

      function f1()
        character(3) f1
        f1 = 'XyX'
      end function
