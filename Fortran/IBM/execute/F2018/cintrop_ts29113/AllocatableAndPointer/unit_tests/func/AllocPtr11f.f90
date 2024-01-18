!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb, 2013
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Allocatable/Pointer
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Move_alloc
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine test(al1, al2) bind(c)
          import
          integer(c_int), allocatable :: al1(:,:), al2(:,:)
        end
      end interface

      integer(c_int), allocatable :: al1(:,:), al2(:,:)
      integer :: i
      allocate(al1(2:11, -1:3))
      al1 = reshape([(i, i=1,10*5,1)], [10, 5])

      call test(al1, al2)

      end

      subroutine test(a, b) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none

        ! Arguments
        integer(c_int), allocatable :: a(:,:), b(:,:)

        ! Locals
        integer :: i, j
        integer(c_int), allocatable :: c(:,:)

        if (.not. allocated(a)) then
           print *, "array 'a' must be allocated"
           error stop 10
        end if

        ! ****** Test 1: ****** !
        print *, "~~~ Test 1:"
        print *, a
        print *, allocated(a), allocated(b)
        call move_alloc(a, b)
        print *, allocated(a), allocated(b)
        print *, lbound(b)
        print *, ubound(b)
        print *, b

        ! ****** Test 2: ****** !
        print *, "~~~ Test 2:"
        deallocate(b)
        allocate(b(3:12, 0:4))
        b = reshape([(i, i=50,1,-1)], [10, 5])

        print *, allocated(b), allocated(c)
        call move_alloc(b, c)
        print *, allocated(b), allocated(c)
        print *, lbound(c)
        print *, ubound(c)
        print *, c

        ! ****** Test 3: ****** !
        print *, "~~~ Test 3:"
        deallocate(c)
        allocate(c(4:13, 2:6))
        c = reshape([(-i, i=1,50,1)], [10, 5])

        print *, allocated(c), allocated(a)
        call move_alloc(c, a)
        print *, allocated(c), allocated(a)
        print *, lbound(a)
        print *, ubound(a)
        print *, a

    end subroutine

