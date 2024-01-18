!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 03/09/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of function reference that returns
!                               allocatable array for data component in
!                               structure constructor; result is parameterized
!                               derived type.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        integer(8) :: id
    end type

    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        type (A) :: id
    end type

    contains

    function genBaseAlloc8 (r1, n, m, id, p)
        integer, intent(in) :: n,m
        integer(8), intent(in) :: id
        real(8), intent(in) :: r1(n*m)
        procedure(real(8)) :: p

        type(base(8,:)), allocatable :: genBaseAlloc8(:)

!        allocate (genBaseAlloc8(m), source=(/(base(8,n)((/(p(r1((i-1)*n+j)), &
!                j=1,n)/), id=A(id)), i=1,m)/))

        allocate (base(8,n) :: genBaseAlloc8(m))

        do i = 1, m
            genBaseAlloc8(i)%id = A(id)
            do j = 1, n
                genBaseAlloc8(i)%data(j) = p(r1((i-1)*n+j))
            end do
        end do
    end function
end module

module n
use m
    type container (k, n, m)
        integer, kind :: k
        integer, len :: n, m

        type(base(k,n)) :: data(m)
    end type

    type (container(8,:,:)), allocatable :: co1(:)
    real(8) d1(100000)
end module

program dtparamConstr029a
use m
use n
    intrinsic dsqrt

    call random_number(d1)

!    allocate (co1(10), source=(/(container(8,400,25) &
!        (genBaseAlloc8(d1((i-1)*10000+1:(i*10000)),400, 25, i*1_8, dsqrt)), &
!        i=1,10)/))

    allocate (container(8,400,25) :: co1(10))

    do i = 1, 10
        co1(i)%data = &
            genBaseAlloc8(d1((i-1)*10000+1:(i*10000)),400, 25, i*1_8, dsqrt)
    end do


    call verifyCo1
end


subroutine verifyCo1
use n
    implicit none

    logical(4), external :: precision_r8

    integer i, j, k, l

    if (size(co1) /= 10) error stop 1_4

    if ((co1%n /= 400) .or. (co1%m /= 25)) error stop 2_4

    l = 1
    do i = 1, 10
        do j = 1, 25
            if (co1(i)%data(j)%id%id /= i) error stop 3_4

            do k = 1, 400
                if (.not. precision_r8(co1(i)%data(j)%data(k), sqrt(d1(l)))) &
                    error stop 4_4

                l = l + 1
            end do
        end do
    end do
end
