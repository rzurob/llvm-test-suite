!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrf032.f
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
!*  TEST CASE TITLE            : charvalueattrf032
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
!*  DESCRIPTION                : Testing association of a runtime length
!*                               character with a dummy arg of another subroutine
!*                               that has the VALUE attribute. This tests
!*                               external procedures and character of len = 3.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main
      
      character(3) :: x
      x = "AaA"
      call s1(x,3)
      
      if( x .ne. 'AaA' ) error stop 4

      call s1("AaA",3)

end program

        subroutine s1(c,i)
          integer :: i
          character(i) :: c !<= character with runtime length
          interface
             subroutine s2(x)
               character(3), value :: x
             end subroutine s2
          end interface
          call s2(c)
          if ( c .ne. 'AaA' ) error stop 1
        end subroutine
        subroutine s2(a)
          character(3), value :: a
          if ( a .ne. 'AaA' ) error stop 2
          if ( len(a) .ne. 3 ) error stop 3
          a = 'QqQ'
        end subroutine
