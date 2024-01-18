!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrf051.f
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
!*  DESCRIPTION                : Test that pass-by-value using VALUE attribute
!*                               works as expected for characters of len > 1.
!*                               Don't check the passed value before changing it.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c)
           character*3, value :: c
         end subroutine
         subroutine s2(d)
           character*87, value :: d
         end subroutine
      end interface

      character(3) x
      character(87) xx

      x = 'AbC'

      call s1(x)

      if( x .ne. 'AbC' ) error stop 1

      xx = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzABCDEFGHI'

      call s2(xx)

      if( xx .ne. 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzABCDEFGHI' ) then
         error stop 2
      endif

      end


      subroutine s1(arg)
        character(3), value :: arg
        arg = 'xYz'
      end subroutine s1

      subroutine s2(arg)
        character(87), value :: arg
        arg = 'xYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYz'
      end subroutine s2
