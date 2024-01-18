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
!*  DESCRIPTION                : Test interoperability of characters with
!*                               length 1 that have VALUE attr. with C.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(a)
           character(1), value :: a
         end subroutine s1
         subroutine s2(b)
           character(1) :: b
         end subroutine s2
      end interface

      character(1) :: x

      x = 'A'

      call s1(x)

      if( x .ne. 'A' ) error stop 1

      call s2(x)

      if( x .ne. 'z' ) error stop 2

      call s1('A')

      end
