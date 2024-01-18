!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrf036.f
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
!*  DESCRIPTION                : Test VALUE attr in external procs.
!*                               This tests characters of lengh 87.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main

      character(87) :: x
      x = "AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA"
      call s1(x)

      if( x .ne. repeat('AaA', 29) ) error stop 4

      call s1("AaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaAAaA")


end program

        subroutine s1(c)
          character(87) :: c
          interface
             subroutine s2(x)
               character(87), value :: x
             end subroutine
          end interface
          call s2(c)
          if ( c .ne. repeat('AaA',29) ) error stop 1
        end subroutine
        subroutine s2(a)
          character(87), value :: a
          if ( a .ne. repeat('AaA', 29) ) error stop 2
          if ( len(a) .ne. 87 ) error stop 3
          a = repeat('QqQ', 29)
        end subroutine
