!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrf055.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 26, 2006
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
!*                               works as expected for characters of len=1.
!*                               This tests when literals are passed to procs.
!*                               Testing module procs.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      module m
      implicit none
      contains
      subroutine s1(arg)
        value arg
        character(len=1) arg

        if( arg .ne. 'A' ) then
          error stop 1
        endif

        arg = 'z'

      end subroutine s1

      end module

      use m

      call s1('A')

      end
