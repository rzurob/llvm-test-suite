! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/funcRetFinal002.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/9/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Finalization of function results after the
!                               intrinsic assignment; test rank-two allocatable
!                               array function results.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)         x(2)

        contains

        final :: finalizePointRank2
        procedure :: distribute
        procedure :: rotate
        procedure :: equal
    end type

    contains

    subroutine finalizePointRank2 (p1)
        type(point(*,4)), intent(inout) :: p1(:,:)

        print *, 'finalizePointRank2'

        print *, lbound(p1), '&', ubound(p1)

        p1%x(1) = sqrt(-1.0)
        p1%x(2) = sqrt(-1.0)
    end subroutine


    !! this function allocate 2 dimensional array of shape (m,n) and rotates the
    !point by 2*pi (rotates a full circle) at a step of 2*pi/(m*n)
    function distribute (p1, m, n)
        implicit none
        class(point(*,4)), intent(in) :: p1
        integer, intent(in) :: m,n

        type(point(:,4)), allocatable :: distribute (:,:)

        real radius, theta, pi, delta

        integer i,j, k

        parameter (pi = atan2(1.0, 0.0)*2.0)

        radius = sqrt(sum(p1%x*p1%x))

        if (radius < 1.0e-5) then
            print *, 'too close to zero'
            stop 10
        end if

        theta = atan2 (p1%x(2), p1%x(1))

        delta = 2*pi/(m*n)

        allocate(point(20,4) :: distribute(m,n))

        k = 1

        do j = 1, n
            do i = 1, m
                distribute(i,j) = point(20,4)(radius*[cos(theta+delta*k), &
                    sin(theta+delta*k)])

                k = k + 1
            end do
        end do
    end function

    subroutine rotate (p1, delta)
        class (point(*,4)), intent(inout) :: p1
        real, intent(in) :: delta

        radius = sqrt(sum(p1%x**2))

        if (radius < 1.0e-5) stop 20

        theta = atan2(p1%x(2), p1%x(1)) + delta

        p1%x = radius*[cos(theta), sin(theta)]
    end subroutine

    logical function equal (p1, p2)
        class(point(*,4)), intent(in) :: p1, p2

        equal = precision_r4(p1%x(1), p2%x(1)) .and. &
                precision_r4(p1%x(2), p2%x(2))

        contains

        logical function precision_r4 (r1, r2)
            implicit none
            real, intent(in) :: r1, r2

            real, parameter :: tol = 1.5e-5

            precision_r4 = abs(r1-r2) <= abs(r1+r2)*tol/2.0
        end function
    end function
end module

program funcRetFinal002
use m
    type(point(:,4)), allocatable :: p1(:,:)
    type(point(:,4)), pointer :: p2(:,:)

    type(point(:,4)), allocatable :: ref

    real, parameter :: pi = atan2(1.0, 0.0)*2.0

    allocate (point(20,4) :: p2(4, 5))

    !! test 1: finalize p2
    print *, 'start'

    p2 = point(20,4)(1.0)

    !! test2: no finalization
    print *, 2

    p1 = p2

    !! test3: finalize p2, then assignment; deallocation of function result in
    !the end
    print *, 3

    p2 = p2(2,4)%distribute(4,5)

    !! test 4: reallocation of p1 due to shape mismatch; 2 finalizations to
    !happen: 1st is during deallocation of p1; 2nd is due to deallocation of
    !function result
    print *, 4
    p1 = p1(3,5)%distribute(5, 6)

    !! verify p1 and p2
    if (any(lbound(p1) /= 1) .or. any(ubound(p1) /= [5,6])) error stop 1_4

    if (any(lbound(p2) /= 1) .or. any(ubound(p2) /= [4,5])) error stop 2_4

    ref = point(20,4)(1.0)

    delta = pi/15

    do j = 1, 6
        do i = 1, 5
            call ref%rotate(delta)

            if (.not. ref%equal(p1(i,j))) error stop 3_4
        end do
    end do

    if (.not. ref%equal(point(20,4)(1.0))) error stop 4_4


    ref = point(20,4)(1.0)

    delta = pi*0.1

    do j = 1, 5
        do i = 1, 4
            call ref%rotate(delta)

            if (.not. ref%equal(p2(i,j))) error stop 5_4
        end do
    end do

    if (.not. ref%equal(point(20,4)(1.0))) error stop 6_4


    !! test 5: finalization of p1 happens; no reallocation; the function result
    !will also be deallocated (finalized as well)
    print *, 5
    p1 = p1(5,3)%distribute(5,6)

    print *, 'end'

    if (any(lbound(p1) /= 1) .or. any(ubound(p1) /= [5,6])) error stop 6_4

    ref = point(20,4)(1.0)
    call ref%rotate (pi)

    delta = pi/15

    do j = 1, 6
        do i = 1, 5
            call ref%rotate(delta)

            if (.not. p1(i,j)%equal(ref)) error stop 7_4
        end do
    end do
end
