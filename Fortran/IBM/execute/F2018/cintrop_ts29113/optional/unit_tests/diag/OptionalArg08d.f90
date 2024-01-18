!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 12, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostics for C-interop OPTIONAL argument
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Diagnostic test for when a BIND(C)
!*                               optional argument is used in an ENTRY
!*                               statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      subroutine foo(a)
        implicit none
        integer, optional :: a
      entry e1f(a)
        print *, present(a)
      entry e2f(a)  bind(c) ! Needs to be caught
        print *, present(a)
      entry e3f(a)
        print *, present(a)
      end

      subroutine bar(a, b) bind(c)
        implicit none
        real, optional :: b
        real :: a
      entry e1b(b) ! Needs to be caught
        print *, present(b)
      entry e2b(b) ! Needs to be caught
        print *, present(b)
      entry e3b(b) ! Needs to be caught
        print *, present(b)
      end

      function func1(a, b, c)
        implicit none
        integer :: func1, e1f1, e2f1, e3f1, e4f1, e5f1, e6f1, r7f1
        integer :: a, b, c
        real :: e, f
        optional :: f
        optional :: b
        integer, optional :: g
        func1 = 0
      entry e1f1(b) bind(c) ! Needs to be caught
        func1 = 1
      entry e2f1(a, c) bind(c)
        func1 = 2
      entry e3f1(a, c, b)
        func1 = 3
      entry e4f1(a, c, b) bind(c) ! Needs to be caught
        func1 = 4
      entry e5f1(a, e) bind(c)
        func1 = 5
      entry e6f1(a, f) bind(c) ! Needs to be caught
        func1 = 6
      entry e7f1(g) bind(c) result(r7f1) ! Needs to be caught
        r7f1 = 7
      end function


      ! we shouldn't flag any of the the following ENTRYs
      subroutine zap(a, b) bind(c)
        implicit none
        real, optional :: b
        real :: a
      entry e1z(a)
        print *, present(b)
      entry e2z(a)
        print *, present(b)
      entry e3z(a)
        print *, present(b)
      end

