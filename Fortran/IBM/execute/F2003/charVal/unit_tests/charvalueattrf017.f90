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
!*  DESCRIPTION                : Access substrings of a character object
!*                               with VALUE attribute.
!*                               This tests characters of len = 1.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1)
           character(1), value :: c1
         end subroutine
      end interface

      character(1) :: c

      c = 'A'

      call s1(c)

      if( c .ne. 'A' ) error stop 2

      end

      subroutine s1(c1)
        character(1), value :: c1
        if ( c1(1:1) .ne. 'A' ) error stop 1
        c1(1:1) = 'B'
      end subroutine

