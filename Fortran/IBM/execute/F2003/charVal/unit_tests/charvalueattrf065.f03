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
!*                               external procedures with more than 1 arg.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main

      character(1) :: x
      character(3) :: y

      interface
         subroutine s1(c1, c2, i1, i2)
          character(*) :: c1, c2
          integer :: i1, i2
         end subroutine
      end interface

      x = "A"
      y = "BbB"

      call s1(x, y, 1, 3)

      if( x .ne. 'A' ) error stop 7
      if( y .ne. 'BbB' ) error stop 8

      call s1("A", "BbB", 1, 3)


end program


      subroutine s1(c1,c2, i1, i2)
          character(i1) :: c1
          character(i2) :: c2
          integer :: i1, i2
          interface
             subroutine s2(c1,c2)
               character(1), value :: c1
               character(3), value :: c2
             end subroutine
          end interface
          call s2(c1, c2)
          if ( c1 .ne. 'A' ) error stop 1
          if ( c2 .ne. 'BbB' ) error stop 2
      end subroutine
      subroutine s2(a, b)
          character(1), value :: a
          character(3), value :: b
          if ( a .ne. 'A' ) error stop 3
          if ( b .ne. 'BbB' ) error stop 4
          if ( len(a) .ne. 1 ) error stop 5
          if ( len(b) .ne. 3 ) error stop 6
          a = 'Q'
          b = 'zZz'
      end subroutine
