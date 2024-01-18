!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March, 2013
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Allocatable/Pointer
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Check the following two rules:
!*
!*   a) When a Fortran procedure that has an INTENT(OUT) allocatable
!*      dummy argument is invoked by a C function, and the actual
!*      argument in the C function is the address of a C descriptor
!*      that describes an allocated allocatable variable, the variable
!*      is deallocated on entry to the Fortran procedure.
!*   b) When a C function is invoked from a Fortran procedure via an
!*      interface with an INTENT(OUT) allocatable dummy argument, and
!*      the actual argument in the reference to the C function is an
!*      allocated allocatable variable, the variable is deallocated on
!*      invocation (before execution of the C function begins).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine test1(a1, a2) bind(c)
          import
          integer(c_int), allocatable :: a1
          integer(c_int), allocatable, intent(out) :: a2
        end
        subroutine test2(a1, a2) bind(c)
          import
          integer(c_int), allocatable :: a1
          integer(c_int), allocatable, intent(out) :: a2
        end
      end interface

      integer(c_int), allocatable :: al1, al2, al3, al4

      allocate(al1, source=55)
      allocate(al2, source=66)
      call test1(al1, al2)
      if (.not. allocated(al2)) error stop 4
      if (al2 .ne. -66) error stop 5

      allocate(al3, source=77)
      allocate(al4, source=88)
      call test2(al3, al4)
      if (.not. allocated(al4)) error stop 9
      if (al4 .ne. -88) error stop 10

      end

      subroutine test1(a1, a2) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), allocatable :: a1
        integer(c_int), allocatable, intent(out) :: a2
        if (.not. allocated(a1)) error stop 1
        if (a1 .ne. 55) error stop 2
        if (allocated(a2)) error stop 3
        allocate(a2)
        a2 = -66
      end

      function test_allocated(a) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), allocatable :: a
        integer(c_short) :: test_allocated
        if (allocated(a)) then
           test_allocated = 1
        else
           test_allocated = 0
        end if
      end

