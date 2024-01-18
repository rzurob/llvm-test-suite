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
!*                               works as expected for characters of len=1.
!*                               This tests when literals are passed to procs.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c)
           character*1, value :: c
         end subroutine
      end interface

      call s1('A')

      end


      subroutine s1(arg)
        value arg
        character(len=1) arg

        if( arg .ne. 'A' ) then
          error stop 1
        endif

        arg = 'z'

      end subroutine s1
