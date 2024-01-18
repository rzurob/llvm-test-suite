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
!*  DATE                       : 04/19/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               structure constructor: Test the dynamic type,
!                               bounds and values of the allocated allocatable
!                               component that is declared to be polymorphic;
!                               component source is allocatable array.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        real(k), allocatable :: data(:)
    end type

    type, extends(base) :: child(n, m)
        integer, len :: n, m

        complex(k) :: cx(n,m)
    end type
end module

module n
use m
    type container (k)
        integer, kind :: k

        class(base(k)), allocatable :: data(:)
    end type
end module

program dtparamConstr058
use n
    integer, parameter :: double = 8
    integer, parameter :: default = 4
    integer, parameter :: extended = 16

    class(base(double)), allocatable :: b1(:)
    class(base(default)), allocatable :: b2(:)
    type(child(extended,:,:)), allocatable :: c1(:)

    type(container(double)) co1
    type(container(default)) co2(10)
    type(container(extended)) co3(2,2)

    logical(4), external :: precision_r4, precision_r8, precision_r6, &
                            precision_x8, precision_x6, precision_x3

    !! set up b1 and use it for co1
    allocate(b1(0:99))

    do i = 0, 99
        b1(i)%data = [(log(1.0_8*(j+1)), j=1,i+1)]
    end do

    co1 = container(double)(b1)


    !! use b2 in co2
    do i = 1, 10
        allocate (b2(0:i), source=[(child(default, i+1, 2*i+2)(null(), &
            reshape([(cmplx(k,k), k=1,2*(i+1)**2)], [i+1, 2*i+2])), j = 0, i)])
! The following is a workaround of ACE

!        allocate(child(default, i+1, 2*i+2) :: b2(0:i))
!
        do j = 0, i
            b2(j)%data = [(sqrt(k*1.0), k = 0,j)]
!            select type (x => b2(j))
!                type is (child(default,*,*))
!                    x%cx = reshape([(cmplx(k,k), k=1,2*(i+1)**2)], [i+1, 2*i+2])
!            end select
        end do

        call move_alloc(b2,co2(i)%data)
    end do

    !! use c1 in defining co3
    do i = 1, 2
        do j = 1, 2
            if (allocated(c1)) deallocate(c1)

            allocate (child(extended,i,j) :: c1(i+j))

            do k = 1, i + j
                c1(k)%data = [(l, l=1,k)]

                c1(k)%cx = cmplx(k, i+j,kind=extended)
            end do

            co3(i,j) = container(extended)(c1)
        end do
    end do


    !! verify co1, co2 and co3
    if (.not. allocated(co1%data)) error stop 100_4

    if (.not. same_type_as(co1%data, base(double)(null()))) error stop 1_4

    if ((lbound(co1%data,1) /= 0) .or. (ubound(co1%data,1) /= 99)) error stop 2_4

    do i = 0, 99
        do j = 1, i+1
            if (.not. precision_r8(co1%data(i)%data(j), log(1.0_8*(j+1)))) &
                    error stop 3_4
        end do
    end do


    !! co2
    do i = 1, 10
        if (.not. allocated(co2(i)%data)) error stop 4_4

        if (.not. same_type_as(co2(i)%data, child(default,i+1,2*i+2)(null(),&
            0))) error stop 5_4

        if ((lbound(co2(i)%data,1) /= 0) .or. (ubound(co2(i)%data,1) /= i)) &
            error stop 6_4

        select type (x => co2(i)%data)
            type is (child(default,*,*))
                do j = 0, i
                    do k = 0, j
                        if (.not. precision_r4(x(j)%data(k+1), &
                            sqrt(k*1.0_4))) error stop 7_4
                    end do

                    l = 1

                    do k2 = 1, 2*i+2
                        do k1 = 1, i+1
                            if (.not. precision_x8(x(j)%cx(k1,k2), &
                                cmplx(l,l,kind=4))) error stop 8_4

                            l = l + 1
                        end do
                    end do
                end do

            class default
                error stop 9_4
        end select
    end do


    !! co3
    do i = 1, 2
        do j = 1, 2
            if (.not. allocated(co3(i,j)%data)) error stop 10_4

            if (.not. same_type_as(co3(i,j)%data, &
                child(extended, i,j)(null(),0))) error stop 11_4

            if ((lbound(co3(i,j)%data,1) /= 1) .or. &
                (ubound(co3(i,j)%data,1) /= i+j)) error stop 12_4

            select type (x => co3(i,j)%data)
                type is (child(extended, *,*))
                    do k = 1, i+j
                        do l = 1, k
                            if (.not. precision_r6(x(k)%data(l), l*1.0_16)) &
                                error stop 13_4
                        end do

                        do l2 = 1, j
                            do l1 = 1, i
                                if (.not. precision_x3(x(k)%cx(l1,l2), &
                                    cmplx(k, i+j,kind=extended))) error stop 14_4
                            end do
                        end do
                    end do

                class default
                    error stop 15_4
            end select
        end do
    end do
end
