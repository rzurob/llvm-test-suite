! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/argAssociation/procPtr002.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/13/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the case of procedure pointer for a
!                               function that returns derived type; use
!                               reference to proc-ptr function as the expr for
!                               an intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: i(:)
        procedure(genBaseAlloc), nopass, pointer :: gen => null()
    end type

    abstract interface
        function genBaseAlloc (b)
        import
            class(base(*,4)), intent(in) :: b(:,:)

            type(base(:,4)), allocatable :: genBaseAlloc(:,:)
        end function
    end interface

    contains

    subroutine test1 (b1, f, b2)
        type(base(:,4)), allocatable :: b1(:,:)
        procedure(genBaseAlloc), pointer, intent(in) :: f
        type(base(*,4)), intent(in) :: b2(:,:)

        if (associated(f)) b1 = f(b2)
    end subroutine
end module

program procPtr002
use m
    procedure(genBaseAlloc) reverseOrder, doubleVal

    type(base(:,4)), allocatable :: b1, b2(:,:)
    class(base(:,4)), allocatable :: b3(:,:)

    b1 = base(20,4)(null(), reverseOrder)

    allocate (base(20,4) :: b3(0:9, 2:11))

    do i = 0, 9
        do j = 2, 11
            allocate (b3(i,j)%i(0:i+j-1))

            b3(i,j)%i = (/(k, k=1,i+j)/)

            b3(i,j)%gen => reverseOrder
        end do
    end do

    ! test1: call reverseOrder
    call test1(b2, b1%gen, b3)

    if (.not. allocated(b2)) error stop 1_4

    if (any(lbound(b2) /= 1) .or. (any(ubound(b2) /= 10))) error stop 2_4

    do i = 1, 10
        do j = 1, 10
            do k = 1, i+j
                if (b2(i,j)%i(k) /= i+j+1-k) error stop 3_4
            end do

            if (.not. associated(b2(i,j)%gen, reverseOrder)) error stop 4_4
        end do
    end do

    b1%gen => doubleVal

    ! test 2 : call doubleVal
    call test1 (b2, b1%gen, b3(:,::2))

    if (.not. allocated(b2)) error stop 5_4

    if (any(lbound(b2) /= 1) .or. any(ubound(b2) /= (/10, 5/))) error stop 6_4

    do i = 1, 10
        do j = 1, 5
            do k = 0, i+2*j-2
                if (b2(i,j)%i(k) /= (k+1)*2) error stop 7_4
            end do
        end do
    end do
end


!! this function reveses the element order for each element of b; the bounds are
!not kept
function reverseOrder (b)
use m
    class(base(*,4)), intent(in) :: b(:,:)

    type(base(:,4)), allocatable :: reverseOrder (:,:)

    reverseOrder = reshape((/(base(20,4)(null()), i=1,size(b))/), shape(b))

    do i = 1, size(b,1)
        do j = 1, size(b,2)
            reverseOrder(i,j)%i = b(i,j)%i(ubound(b(i,j)%i,1):lbound(b(i,j)%i,1):-1)

            reverseOrder(i,j)%gen => b(i,j)%gen
        end do
    end do
end function


!! this function doubles the value for each element for the b's components; but
!it keeps the same bounds for each element of b
function doubleVal (b) result (results)
use m
    class(base(*,4)), intent(in) :: b(:,:)
    type(base(:,4)), allocatable :: results(:,:)

    results = b

    do i = 1, size(b,1)
        do j = 1, size(b,2)
            results(i,j)%i = b(i,j)%i * 2
        end do
    end do
end function
