!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 26, 2006
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
!*                               This tests when literals are passed to procs.
!*                               Test VALUE stmt and internal procs.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      call s1('AbC')

      call s2('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzABCDEFGHI')

      contains
      subroutine s1(arg)
        value :: arg
        character(3) :: arg

        if( arg .ne. 'AbC' ) then
          error stop 1
        endif

        arg = 'xYz'

      end subroutine s1

      subroutine s2(arg)
        character(87) :: arg
        value :: arg

        if( arg .ne. 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzABCDEFGHI' ) then
          error stop 2
        endif

        arg = 'xYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYz'

      end subroutine s2

      end