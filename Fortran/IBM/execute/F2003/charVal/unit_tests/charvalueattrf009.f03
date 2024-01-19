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
!*  DESCRIPTION                : Test concatenation of characters with VALUE attribute
!*                               Test characters of length 3.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1, c2)
           character(3), value :: c1, c2
         end subroutine
      end interface

      character(3) x, y

      x = 'AaA'
      y = 'bBb'

      call s1(x, y)

      if( x .ne. 'AaA' ) error stop 7
      if( y .ne. 'bBb' ) error stop 8

      end


      subroutine s1(arg1, arg2)

        character(3), value :: arg1, arg2
        character(6) :: tmp

        if ( (arg1 // arg2) .ne. "AaAbBb" ) error stop 1

        tmp = arg2 // arg1

        if ( tmp .ne. "bBbAaA" ) error stop 2

        if ( (arg1 // arg1) .ne. "AaAAaA" ) error stop 3

        tmp = arg2 // arg2

        if ( tmp .ne. "bBbbBb" ) error stop 4

        if ( (arg2 // "qxq") .ne. "bBbqxq" ) error stop 5

        tmp = "zxz" // arg1

        if ( tmp .ne. "zxzAaA" ) error stop 6

      end subroutine s1

