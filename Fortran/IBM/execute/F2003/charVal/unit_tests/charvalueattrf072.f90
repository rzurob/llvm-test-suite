!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: charvalueattrf072.f
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
!*                               1 arg and the actual arg is a function
!*                               result longer than the dummy
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program main

      character(1) :: c1
      character(2) :: c2
      character(3) :: c3
      character(87) :: c4

      c1 = 'A'
      c2 = 'AB'
      c3 = 'ABC'
      c4 = repeat('ABC',29)

!     s1
      call s1(f2(), c1)
      if ( c1 .ne. 'A' ) error stop 20

      call s1(f3(), c1)
      if ( c1 .ne. 'A' ) error stop 21

      call s1(f3(), c2 )
      if ( c2 .ne. 'AB' ) error stop 22

      call s1(f4(), c3)
      if ( c3 .ne. 'ABC' ) error stop 23

      call s1(f4(), c4 )
      if ( c4 .ne. repeat('ABC',29)) error stop 24

!     s2
      call s2(f2(), c2)
      if ( c2 .ne. 'AB' ) error stop 25

      call s2(f3(), c2)
      if ( c2 .ne. 'AB' ) error stop 26

      call s2(f3(), c3 )
      if ( c3 .ne. 'ABC' ) error stop 27

      call s2(f4(), c4 )
      if ( c4 .ne. repeat('ABC',29)) error stop 28

!     s3
      call s3(f2(), c3)
      if ( c3 .ne. 'ABC' ) error stop 29

      call s3(f3(), c3 )
      if ( c3 .ne. 'ABC' ) error stop 30

      call s3(f4(), c4 )
      if ( c4 .ne. repeat('ABC',29)) error stop 31

!     s4

      call s4(f2(), c4 )
      if ( c4 .ne. repeat('ABC',29) ) error stop 32

      call s4(f3(), c4 )
      if ( c4 .ne. repeat('ABC',29) ) error stop 33

      call s4(f4(), c4 )
      if ( c4 .ne. repeat('ABC',29)) error stop 34

!     s11
      call s11(f2(), c1)
      if ( c1 .ne. 'A' ) error stop 35

      call s11(f3(), c1)
      if ( c1 .ne. 'A' ) error stop 36

      call s11(f3(), c2 )
      if ( c2 .ne. 'AB' ) error stop 37

      call s11(f4(), c3)
      if ( c3 .ne. 'ABC' ) error stop 38

      call s11(f4(), c4 )
      if ( c4 .ne. repeat('ABC',29)) error stop 39

!     s22
      call s22(f2(), c2)
      if ( c2 .ne. 'AB' ) error stop 40

      call s22(f3(), c2)
      if ( c2 .ne. 'AB' ) error stop 41

      call s22(f3(), c3 )
      if ( c3 .ne. 'ABC' ) error stop 42

      call s22(f4(), c4 )
      if ( c4 .ne. repeat('ABC',29)) error stop 43

!     s33
      call s33(f2(), c3)
      if ( c3 .ne. 'ABC' ) error stop 44

      call s33(f3(), c3 )
      if ( c3 .ne. 'ABC' ) error stop 45

      call s33(f4(), c4 )
      if ( c4 .ne. repeat('ABC',29)) error stop 45

!     s4

      call s44(f2(), c4 )
      if ( c4 .ne. repeat('ABC',29) ) error stop 46

      call s44(f3(), c4 )
      if ( c4 .ne. repeat('ABC',29) ) error stop 47

      call s44(f4(), c4 )
      if ( c4 .ne. repeat('ABC',29)) error stop 48


      contains
      function f1()
        character(1) :: f1
        f1 = 'a'
      end function
      function f2()
        character(3) :: f2
        f2 = 'abc'
      end function
      function f3()
        character(7) :: f3
        f3 = 'abcdefg'
      end function
      function f4()
        character(87) :: f4
        f4 = repeat('abc',29)
      end function

      subroutine s1(arg1, arg2)
        character(3), value :: arg1
        character(1), value :: arg2
        if ( arg1 .ne. 'abc' ) error stop 1
        if ( arg2 .ne. 'A' ) error stop 2
        arg1 = 'xyz'
        arg2 = 'q'
      end subroutine
      subroutine s2(arg1, arg2)
        character(3), value :: arg1
        character(2), value :: arg2
        if ( arg1 .ne. 'abc' ) error stop 3
        if ( arg2 .ne. 'AB' ) error stop 4
        arg1 = 'xyz'
        arg2 = 'qr'
      end subroutine
      subroutine s3(arg1, arg2)
        character(3), value :: arg1
        character(3), value :: arg2
        if ( arg1 .ne. 'abc' ) error stop 5
        if ( arg2 .ne. 'ABC' ) error stop 6
        arg1 = 'xyz'
        arg2 = 'qr'
      end subroutine
      subroutine s4(arg1, arg2)
        character(3), value :: arg1
        character(87), value :: arg2
        if ( arg1 .ne. 'abc' ) error stop 7
        if ( arg2 .ne. repeat('ABC',29) ) error stop 8
        arg1 = 'xyz'
        arg2 = repeat('qrs',29)
      end subroutine
      subroutine s11(arg1, arg2)
        character(1), value :: arg1
        character(1), value :: arg2
        if ( arg1 .ne. 'a' ) error stop 11
        if ( arg2 .ne. 'A' ) error stop 12
        arg1 = 'x'
        arg2 = 'q'
      end subroutine
      subroutine s22(arg1, arg2)
        character(1), value :: arg1
        character(2), value :: arg2
        if ( arg1 .ne. 'a' ) error stop 13
        if ( arg2 .ne. 'AB' ) error stop 14
        arg1 = 'x'
        arg2 = 'qr'
      end subroutine
      subroutine s33(arg1, arg2)
        character(1), value :: arg1
        character(3), value :: arg2
        if ( arg1 .ne. 'a' ) error stop 15
        if ( arg2 .ne. 'ABC' ) error stop 16
        arg1 = 'x'
        arg2 = 'qrs'
      end subroutine
      subroutine s44(arg1, arg2)
        character(1), value :: arg1
        character(87), value :: arg2
        if ( arg1 .ne. 'a' ) error stop 17
        if ( arg2 .ne. repeat('ABC',29) ) error stop 18
        arg1 = 'xyz'
        arg2 = repeat('qrs',29)
      end subroutine

end program

