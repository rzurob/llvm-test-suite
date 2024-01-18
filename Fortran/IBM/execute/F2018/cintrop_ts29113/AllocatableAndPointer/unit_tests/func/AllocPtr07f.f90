!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb, 2013
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Allocatable/Pointer
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Calling TIFs that end up turning into
!*                               calls to the RTE.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine test(ptr1, ptr2) bind(c)
          import
          real(c_double), pointer :: ptr1(:,:), ptr2(:,:)
        end
      end interface

      real(c_double), pointer :: ptr1(:,:), ptr2(:,:)

      ptr1 => NULL()
      ptr2 => NULL()

      call test(ptr1, ptr2)

      end

    subroutine test(a, b) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine print2dfarr(arg, test) bind(c)
          import
          real(c_double), allocatable :: arg(:,:)
          integer(c_int), value :: test
        end
        subroutine initfptr(arg, stride, src) bind(c)
          import
          real(c_double), pointer :: arg(:,:)
          integer(c_int), optional :: stride
          real(c_double), pointer :: src(:,:)
        end

      end interface

      ! Arguments
      real(c_double), pointer :: a(:, :)
      real(c_double), pointer :: b(:, :)

      ! Local variables
      real(c_double), allocatable :: res(:,:)
      real(c_double), pointer :: src(:,:)
      real(c_double), target  :: t1(6,2)
      real(c_double), target  :: t2(6,3)

      t1(1,1) = 1.0; t1(1,2) = 2.0;
      t1(2,1) = 2.0; t1(2,2) = 5.0;
      t1(3,1) = 5.0; t1(3,2) = 3.0;
      t1(4,1) = 6.0; t1(4,2) = -8.0;
      t1(5,1) = 9.0; t1(5,2) = 2.0;
      t1(6,1) = 7.0; t1(6,2) = 10.0;

      t2(1,1) = 7.0; t2(1,2) = 1.0; t2(1,3) = 4.0;
      t2(2,1) = 0.0; t2(2,2) = 5.0; t2(2,3) = 0.0;
      t2(3,1) = 5.0; t2(3,2) = 0.0; t2(3,3) = 5.0;
      t2(4,1) = 3.0; t2(4,2) = 2.0; t2(4,3) = 1.0;
      t2(5,1) = -1.0; t2(5,2) = -1.0; t2(5,3) = -1.0;
      t2(6,1) = 0.0; t2(6,2) = 0.0; t2(6,3) = 8.0;

      ! ****** Test 1: ****** !
      ! Call a C function to associate the pointer
      src => t1
      call initfptr(a, 2, src)
      src => t2
      call initfptr(b, 3, src)

      if (.not. associated(a)) error stop 1
      if (.not. associated(b)) error stop 2
      if (is_contiguous(a)) error stop 3
      if (is_contiguous(b)) error stop 4

      allocate(res(ubound(a,1)-lbound(a,1)+1, ubound(b,2)-lbound(b,2)+1))

      res = matmul(a, b)

      call print2dfarr(res, 1)
      deallocate(res)

      ! ****** Test 2: ****** !
      ! Make the destination initially associated.
      a => t2
      b => t1
      ! Call a C function to associate the pointer
      src => t1(::2,:)
      call initfptr(a, src=src) ! no strides this time

      src => t2(::3,:)
      call initfptr(b, src=src) ! no strides this time

      if (.not. associated(a)) error stop 5
      if (.not. associated(b)) error stop 6
      if (is_contiguous(a)) error stop 7
      if (is_contiguous(b)) error stop 8

      allocate(res(ubound(a,1)-lbound(a,1)+1, ubound(b,2)-lbound(b,2)+1))

      res = matmul(a, b)

      call print2dfarr(res, 2)

      deallocate(res)

    end subroutine

