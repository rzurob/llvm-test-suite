!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/01/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: C481; part 1: Try dummy-arg that has
!                               assumed length type parameter, and is
!                               allocatable.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k,n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    interface update
        module procedure updateBase4
        module procedure updateBase8
    end interface

    contains

    subroutine updateBase4 (b, r1)
        class(base(4,n=*)), allocatable, intent(out) :: b
        real(4), intent(in) :: r1(b%n)

        allocate (b)
        b%data = r1
    end subroutine

    subroutine updateBase8 (b, r1)
        class(base(n=*, k=8)), allocatable, intent(out) :: b
        real(8), intent(in) :: r1(b%n)

        allocate (b)
        b%data = r1
    end subroutine
end module

program dtSpec006
use m
    class(base(4, n=100)), allocatable :: b1
    class(base(n=250, k=8)), allocatable :: b2

    real(4) r1(200)
    double precision d1(1000)

    logical(4), external :: precision_r4, precision_r8

    allocate (b1)
    allocate (base(8,250):: b2)

    r1 = (/(i*1.0, i=1, 200)/)

    d1 = (/(dcos(i*1.0d0)*dsin(i*1.0d0), i = 1, 1000)/)

    call update (b1, r1(::2))
    call update (b2, d1)

    !! verify
    do i = 1, 100
        if (.not. precision_r4(b1%data(i), (2*i-1)*1.0)) error stop 1_4
    end do

    do i = 1, 250
        if (.not. precision_r8(b2%data(i), dcos(i*1.0d0)*dsin(i*1.0d0))) &
            error stop 2_4
    end do
end
