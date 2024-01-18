!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrf044.f
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
!*  TEST CASE TITLE            : charvalueattrf044
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
!*  DESCRIPTION                : Testing VALUE attr in module procs.
!*                               This tests characters of length 3.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
      contains
        subroutine s1(c)
          character(3) :: c
          call s2(c)
          if ( c .ne. 'AaA' ) error stop 1
        end subroutine
        subroutine s2(a)
          character(3), value :: a
          if ( a .ne. 'AaA' ) error stop 2
          if ( len(a) .ne. 3 ) error stop 3
          a = 'QqQ'
        end subroutine
end module
program main
      use m
      character(3) :: x
      x = "AaA"
      call s1(x)
      
      if( x .ne. 'AaA' ) error stop 4

      call s1("AaA")

end program

