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
!*  DESCRIPTION                : Testing association of a runtime length
!*                               character with a dummy arg of another subroutine
!*                               that has the VALUE attribute. This tests
!*                               internal procedures and character of len = 1.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main

      character(1) :: x
      x = "A"
      call s1(x,1)

      if( x .ne. 'A' ) error stop 4

      call s1("A",1)

      contains
        subroutine s1(c, i)
          character(i) :: c !<= runtime length
          integer :: i
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