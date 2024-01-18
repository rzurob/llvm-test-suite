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
!*  DESCRIPTION                : Pointer Assignment
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine test(p1, p2) !bind(c)
          import
          integer(c_int), pointer :: p1(:,:), p2
        end
        subroutine test2(p1, p2) !bind(c)
          import
          integer(c_int), pointer, intent(out) :: p1(:)
          integer(c_int), pointer, intent(in) :: p2(:)
        end
      end interface

      integer(c_int), pointer :: parr(:,:), pscal
      integer(c_int), pointer :: ptarg(:), pdest(:)

      parr => NULL()
      pscal => NULL()
      call test(parr, pscal)

      allocate(ptarg(12), source=[1,2,3,4,5,6,7,8,9,10,11,12])
      call test2(pdest, ptarg)
      print *, associated(pdest)
      print *, pdest

      end

      subroutine test(a, b) !bind(c)
        use, intrinsic :: iso_c_binding
        implicit none

        ! Arguments
        integer(c_int), pointer :: a(:,:), b

        ! Locals
        integer(c_int), target :: copy(5,2)
        integer(c_int), pointer :: lp1(:), lp2(:,:), lp
        integer :: i, j

        lp1 => NULL()
        lp2 => NULL()
        lp => NULL()
        copy = reshape([(i,i=1,10,1)], [5,2])

        if (associated(a) .or. associated(b)) error stop 20

        ! ****** Test 1: ****** !
        print *, "~~~ Test 1:"
        a => copy
        print *, associated(a)
        print *, a
        print *, lbound(a), ubound(a)
        if (.not. is_contiguous(a)) error stop 1
        a => NULL()
        print *, associated(a)

        ! ****** Test 2: ****** !
        print *, "~~~ Test 2:"
        b => copy(5,1)
        print *, associated(b)
        print *, b
        b => NULL()
        print *, associated(b)

        ! ****** Test 3: ****** !
        print *, "~~~ Test 3:"
        print *, associated(lp2)
        a => copy
        lp2 => a
        print *, associated(lp2)
        print *, lp2
        print *, lbound(lp2), ubound(lp2)
        if (.not. is_contiguous(a)) error stop 3

        ! ****** Test 4: ****** !
        print *, "~~~ Test 4:"
        b => a(3,2)
        print *, associated(b)
        print *, b

        ! ****** Test 5: ****** !
        print *, "~~~ Test 5:"
        lp1 => a(:,2)
        print *, associated(lp1)
        print *, lp1
        print *, lbound(lp1), ubound(lp1)
        if (.not. is_contiguous(lp1)) error stop 5

        ! ****** Test 6: ****** !
        print *, "~~~ Test 6:"
        lp2(-1:, 2:) => a
        print *, associated(lp2)
        print *, lp2
        print *, lbound(lp2), ubound(lp2)
        if (.not. is_contiguous(lp2)) error stop 6

        ! ****** Test 7: ****** !
        print *, "~~~ Test 7:"
        copy = -copy
        a(-2:, 3:) => copy
        print *, associated(a)
        print *, a
        copy = -copy
        print *, a
        print *, lbound(a), ubound(a)
        if (.not. is_contiguous(a)) error stop 7


        ! ****** Test 8: ****** !
        print *, "~~~ Test 8:"
        a => copy(::2,:)
        print *, associated(a)
        print *, a
        if (is_contiguous(a)) error stop 8
        print *, lbound(a), ubound(a)

        ! ****** Test 9: ****** !
        print *, "~~~ Test 9:"
        b => copy(4,2)
        lp => b
        print *, associated(lp)
        print *, lp

        ! ****** Test 10: ****** !
        print *, "~~~ Test 10:"
        lp2 => NULL()
        allocate(lp2(7,2))
        lp2 = reshape([(i, i=1,14,1)], [7,2])
        a => lp2
        print *, associated(a)
        print *, a

      end subroutine

      subroutine test2(pout, pin) !bind(c)
        use, intrinsic :: iso_c_binding
        implicit none

        integer(c_int), pointer, intent(out) :: pout(:)
        integer(c_int), pointer, intent(in) :: pin(:)

        ! ****** Test 11: ****** !
        print *, "~~~ Test 11:"
        pout => pin
        print *, associated(pout)
        print *, pout


      end
