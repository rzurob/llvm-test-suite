!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrf071.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 01, 2006
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
!*  DESCRIPTION                : test for when the proc takes more than
!*                               1 arg and the actual args are longer than the dummy
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main

      character(5) :: x
      x = 'def'
      call s1('abc', x)
      if ( x .ne. 'def' ) error stop 1
      call s2(x, 'abc')
      if ( x .ne. 'def' ) error stop 2
      contains
      subroutine s1(arg1, arg2)
        character(2), value :: arg1, arg2
        if ( arg1 .ne. 'ab' ) error stop 3
        if ( arg2 .ne. 'de' ) error stop 4
      end subroutine
      subroutine s2(arg1, arg2)
        character(2), value :: arg1, arg2
        if ( arg2 .ne. 'ab' ) error stop 5
        if ( arg1 .ne. 'de' ) error stop 6
      end subroutine

end program

