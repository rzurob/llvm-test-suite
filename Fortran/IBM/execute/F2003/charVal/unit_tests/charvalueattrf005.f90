!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrf005.f
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
!*  TEST CASE TITLE            : charvalueattrf005
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Jan. 25, 2006
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
!*  DESCRIPTION                : Test LEN and INDEX intrinsics when characters with
!*                               VALUE attr are passed to them. Test character of
!*                               length 1.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c)
           character(1), value :: c
         end subroutine
      end interface

      character(1) x

      x = 'A'
      
      call s1(x)

      if( x .ne. 'A' ) error stop 5

      end


      subroutine s1(arg)
        character(1), value :: arg
        integer :: ilen, ix
        
        ilen = len(arg)
        if ( ilen .ne. 1 ) error stop 1

        ix = index(arg, 'A')
        if ( ix .ne. 1 ) error stop 2
        
        if ( index(arg, 'A', back=.true.) .ne. 1 ) error stop 3
        
        if ( index(arg, 'a') .ne. 0 ) error stop 4
        

      end subroutine s1

