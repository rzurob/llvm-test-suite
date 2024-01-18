!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: charvalueattrf075.f
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
!*  TEST CASE TITLE            : charvalueattrf075
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
!*                               1 arg and the actual arg is a substring.
!*                               Try substrings with runtime boundary
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main

      character(10) :: c
      character(3) :: x
      integer :: ii, jj
      c = 'abcdefghij'
      x = 'try'
      
      ii = 3
      jj = 3
      call s1(c(ii:jj), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try' ) error stop 9
      
      ii = 3
      jj = 5
      call s1(c(ii:jj), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 10

      call s1(c(ii:jj),x(1:1))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 11

      jj = 6
      call s1(c(ii:jj),x(1:2))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 12

      jj = 5
      call s2(c(3:jj), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try' ) error stop 13
      
      ii = 3
      call s2(c(ii:8), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 14

      ii = 3
      jj = 7
      call s2(c(ii:jj),x(1:3))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 15

      ii = 3
      jj = 9
      call s2(c(ii:jj),x(1:3))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 16

      jj = 5
      call s3(c(ii:jj), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try' ) error stop 17
      
      jj = 6
      call s3(c(ii:jj), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 18

      jj = 8
      call s3(c(ii:jj),x(1:1))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 19

      jj = 9
      call s3(c(ii:jj),x(1:2))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 20

      jj = 5
      call s4(c(ii:jj), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try' ) error stop 21
      
      jj = 8
      call s4(c(ii:jj), x)
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 22

      jj = 7
      call s4(c(ii:jj),x(1:2))
      if ( c .ne. 'abcdefghij' .or. x .ne. 'try') error stop 23

      jj = 9
      call s4(c(ii:jj),x(1:3))
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

