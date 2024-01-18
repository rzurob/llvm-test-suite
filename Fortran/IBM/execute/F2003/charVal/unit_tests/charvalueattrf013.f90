!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrf013.f
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
!*  TEST CASE TITLE            : charvalueattrf013
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
!*  DESCRIPTION                : Test the association of a function result with
!*                               a character dummy argument with VALUE attribute.
!*                               Test character of length 87.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         subroutine s1(c1)
           character(87), value :: c1
         end subroutine
         function f1()
           character(87) f1
         end function
      end interface

      character(87) :: c
      
      call s1(f1())

      c = f1()
      
      call s1(c)
      
      if ( c .ne. 'XyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyX' ) error stop 3

      end

      subroutine s1(c1)
        character(87), value :: c1
        if ( c1 .ne. 'XyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyX' ) error stop 1
        if ( c1//c1 .ne. repeat('XyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyX', 2) ) error stop 2
        c1 = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
      end subroutine

      function f1()
        character(87) f1
        f1 = 'XyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyXXyX'
      end function
