!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrf073.f
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
!*                               1 arg and the actual arg is a substring
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main

      character(10) :: c
      character(3) :: x
      c = 'abcdefghij'
      x = 'try'

      call s1(c(3:3), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try' ) error stop 9

      call s1(c(3:5), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 10

      call s1(c(3:5),x(1:1))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 11

      call s1(c(3:6),x(1:2))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 12


      call s2(c(3:5), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try' ) error stop 13

      call s2(c(3:8), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 14

      call s2(c(3:7),x(1:3))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 15

      call s2(c(3:9),x(1:3))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 16


      call s3(c(3:5), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try' ) error stop 17

      call s3(c(3:6), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 18

      call s3(c(3:8),x(1:1))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 19

      call s3(c(3:9),x(1:2))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 20


      call s4(c(3:5), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try' ) error stop 21

      call s4(c(3:8), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 22

      call s4(c(3:7),x(1:2))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 23

      call s4(c(3:9),x(1:3))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 24


      contains
      subroutine s1(a1, a2)
        character(1), value :: a1
        character(1), value :: a2
        if( a1 .ne. 'c' ) error stop 1
        if( a2 .ne. 't') error stop 2
        a1 = 'x'
        a2 = 'y'
      end subroutine
      subroutine s2(a1, a2)
        character(1), value :: a1
        character(3), value :: a2
        if( a1 .ne. 'c' ) error stop 3
        if( a2 .ne. 'try') error stop 4
        a1 = 'x'
        a2 = 'yzw'
      end subroutine
      subroutine s3(a1, a2)
        character(3), value :: a1
        character(1), value :: a2
        if( a1 .ne. 'cde' ) error stop 5
        if( a2 .ne. 't') error stop 6
        a1 = 'xyz'
        a2 = 'y'
      end subroutine
      subroutine s4(a1, a2)
        character(3), value :: a1
        character(2), value :: a2
        if( a1 .ne. 'cde' ) error stop 7
        if( a2 .ne. 'tr') error stop 8
        a1 = 'xyz'
        a2 = 'yz'
      end subroutine

end program

