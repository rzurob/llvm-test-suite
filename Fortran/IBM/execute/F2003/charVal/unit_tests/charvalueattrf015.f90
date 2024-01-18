!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrf015.f
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
!*  TEST CASE TITLE            : charvalueattrf015
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
!*  DESCRIPTION                : Test the association of an intrinsic proc result with
!*                               a character dummy argument with VALUE attribute.
!*                               Test character of length 3.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1)
           character(3), value :: c1
         end subroutine
      end interface

      character(3) :: c
      
      call s1(repeat('XyX',1))

      c = repeat('XyX',1)
      
      call s1(c)
      
      if ( c .ne. 'XyX' ) error stop 3

      end

      subroutine s1(c1)
        character(3), value :: c1
        if ( c1 .ne. 'XyX' ) error stop 1
        if ( c1//c1 .ne. 'XyXXyX' ) error stop 2
        c1 = 'aaa'
      end subroutine
