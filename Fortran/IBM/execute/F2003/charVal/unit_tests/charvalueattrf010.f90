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
!*                               Test characters of length 87.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1, c2)
           character(87), value :: c1, c2
         end subroutine
      end interface

      character(87) x, y

      x = 'AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA'
      y = 'bBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBb'

      call s1(x, y)

      if( x .ne. 'AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA' ) then
         error stop 7
      end if
      if( y .ne. 'bBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBb' ) then
         error stop 8
      end if
      end


      subroutine s1(arg1, arg2)

        character(87), value :: arg1, arg2
        character(174) :: tmp

        if ( (arg1 // arg2) .ne. 'AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBb' ) then
           error stop 1
        end if
        tmp = arg2 // arg1

        if ( tmp .ne. 'bBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA' ) then
           error stop 2
        endif
        if ( (arg1 // arg1) .ne. repeat('AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA', 2) ) then
           error stop 3
        endif
        tmp = arg2 // arg2

        if ( tmp .ne. repeat('bBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBb',2) ) then
           error stop 4
        endif
        if ( (arg2 // "qxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxq") .ne. "bBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbbBbqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxqqxq" ) then
           error stop 5
        endif
        tmp = "zxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxz" // arg1

        if ( tmp .ne. "zxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzzxzAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA" ) then
           error stop 6
        endif
      end subroutine s1

