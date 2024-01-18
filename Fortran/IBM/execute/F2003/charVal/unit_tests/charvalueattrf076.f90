!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrf076.f
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
!*  TEST CASE TITLE            : charvalueattrf076
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Feb. 01, 2006
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
!*  DESCRIPTION                : test for when the proc takes more than 
!*                               1 arg and the actual arg is a substring
!*                               and the parent of the substring is of len = 1
!*                               and the bounds are runtime values
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main

      character(1) :: cc
      integer :: ii, jj
      cc = 'a'
      ii = 1
      jj = 1
      call s1(cc(ii:jj), 'XZ')
      if( cc .ne. 'a' ) error stop 3

      contains
      subroutine s1(x,y)
        character(1), value :: x
        character(2), value :: y
        if ( x .ne. 'a' ) error stop 1
        if ( y .ne. 'XZ' ) error stop 2
        x = 'q'
        y = 'rs'
      end subroutine


end program

