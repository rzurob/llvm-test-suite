!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Validate the functionality of the VALUE
!*                               attribute when used with characters of
!*                               length other than 1. ( Feature 298120 )
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing association of an assumed length
!*                               character with a dummy arg of another subroutine
!*                               that has the VALUE attribute. This tests
!*                               internal procedures and character of len = 3.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main

      character(3) :: x
      x = "AaA"
      call s1(x)

      if( x .ne. 'AaA' ) error stop 4

      call s1("AaA")

      contains
        subroutine s1(c)
          character(*) :: c
          call s2(c)
          if ( c .ne. 'AaA' ) error stop 1
        end subroutine
        subroutine s2(a)
          character(3), value :: a
          if ( a .ne. 'AaA' ) error stop 2
          if ( len(a) .ne. 3 ) error stop 3
          a = 'QqQ'
        end subroutine

end program