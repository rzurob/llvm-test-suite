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
!*  DESCRIPTION                : Forall
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
        subroutine test(al1) bind(c)
          import
          integer(c_int), allocatable :: al1(:,:)
        end
      end interface

      integer(c_int), allocatable :: al1(:,:)
      integer :: i

      allocate(al1(5, 5))
      al1 = reshape([(i, i=1,5*5,1)], [5, 5])

      call test(al1)

      end

      subroutine test(a) bind(c)
        use, intrinsic :: iso_c_binding
        implicit none

        ! Arguments
        integer(c_int), allocatable :: a(:,:)

        ! Locals
        integer(c_int), allocatable :: copy(:,:)
        integer :: i, j

        allocate(copy, source=a)

        ! ****** Test 1: ****** !
        print *, "~~~ Test 1:"
        print *, a
        forall(i=lbound(a,1):ubound(a,1), j=lbound(a,2):ubound(a,2), mod(a(i,j),5) .eq. 0)
          a(i,j) = 0
        end forall
        print *, a


        ! ****** Test 2: ****** !
        print *, "~~~ Test 2:"
        forall(i=lbound(copy,1):ubound(copy,1), j=lbound(copy,2):ubound(copy,2), mod(copy(i,j),5) .eq. 0)
          copy(i,j) = -5
          a(i,j) = -5
        end forall

        if (any(copy .ne. a)) error stop 1
        print *, a

    end subroutine

