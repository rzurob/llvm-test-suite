!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrf025.f
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
!*  TEST CASE TITLE            : charvalueattrf025
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
!*  DESCRIPTION                : Testing VALUE attr for internal procs.
!*                               This tests characters of length 1.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main
      
      character(1) :: x
      x = "A"
      call s1(x)
      
      if( x .ne. 'A' ) error stop 4

      call s1("A")

      contains
        subroutine s1(c)
          character(1) :: c
          call s2(c)
          if ( c .ne. 'A' ) error stop 1
        end subroutine
        subroutine s2(a)
          character(1), value :: a
          if ( a .ne. 'A' ) error stop 2
          if ( len(a) .ne. 1 ) error stop 3
          a = 'Q'
        end subroutine

end program
