! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/08/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of function reference that returns
!                               pointer array for data component in structure
!                               constructor; data is of parameterized derived
!                               type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    interface genBasePtr
        module procedure genBasePtr4
        module procedure genBasePtr8
    end interface

    contains

    function genBasePtr4 (r1, n, m, p)
        integer, intent(in) :: n, m
        real(4), intent(in) :: r1(n*m)
        procedure(real(4)) p

        type(base(4,:)), pointer :: genBasePtr4(:)


        allocate (base(4,n) :: genBasePtr4(m))

        do i = 1, m
            do j = 1, n
                genBasePtr4(i)%data(j) = p(r1((i-1)*n+j))
            end do
        end do
    end function

    function genBasePtr8 (d1, n, m, p)
        integer, intent(in) :: n, m
        real(8), intent(in) :: d1(m*n)
        procedure(real(8)) p

        type(base(8,:)), pointer :: genBasePtr8(:)

!        allocate (genBasePtr8(m), source=&
!                (/(base(8,n)((/(p(d1((i-1)*n+j)), j=1,n)/)), i=1, m)/) )
        allocate (base(8,n) :: genBasePtr8(m))

        do i = 1, m
            do j = 1, n
                genBasePtr8(i)%data(j) = p(d1((i-1)*n+j))
            end do
        end do
    end function
end module

module m1
use m, only: base
    type container (k, n, m)
        integer, kind :: k
        integer, len :: n, m

        type (base(k,n)) :: data(m)
    end type

    type (container(4, 35, 7)) :: co1_m
end module

program dtparamConstr028a
use m
use m1
    type (container(8,:,:)), allocatable :: co1

    real(4) r1(250), k, kd
    real(8) d1(8000)

    intrinsic sin, dsqrt

    logical(4), external :: precision_r4, precision_r8

    r1 = (/(i, i=1, 250)/)
    d1 = (/(i*1.0d0, i=1, 8000)/)

    allocate (container(8,442,18) :: co1)

    co1_m = container(4,35,7)(genBasePtr(r1,35,7,sin))

    co1 = container(8,442,18)(genBasePtr(d1,442,18,dsqrt))

    !! verify results
    k = 1

    do i = 1, 7
        do j = 1, 35
            if (.not. precision_r4(co1_m%data(i)%data(j), sin(k*1.0))) &
                    error stop 1_4

            k = k + 1
        end do
    end do


    kd = 1

    do i = 1, 18
        do j = 1, 442
            if (.not. precision_r8(co1%data(i)%data(j), sqrt(kd*1.0d0))) &
                    error stop 2_4

            kd = kd +1
        end do
    end do

    if ((k /= 246) .or. (kd /= 7957)) error stop 3_4
end

