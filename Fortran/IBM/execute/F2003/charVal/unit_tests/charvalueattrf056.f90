!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrf056.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : charvalueattrf056
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Jan. 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the VALUE
!*                               attribute when used with characters of 
!*                               length other than 1. ( Feature 298120 )   
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : Test that pass-by-value using VALUE attribute
!*                               works as expected for characters of len > 1.
!*                               This tests when literals are passed to module procs.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
      implicit none
      contains
      subroutine s1(arg)
        character(3), value :: arg

        print*, arg
        if( arg .ne. 'AbC' ) then
          error stop 1
        endif
        
        arg = 'xYz'
        
      end subroutine s1

      subroutine s2(arg)
        character(87), value :: arg

        if( arg .ne. 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzABCDEFGHI' ) then
          error stop 2
        endif
        
        arg = 'xYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYzxYz'
        
      end subroutine s2

end module
      
      use m

      call s1("AbC")

      call s2('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzABCDEFGHI')
      
      end
