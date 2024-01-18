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
!*  DATE                       : 08/16/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               Use function result that is an array of derived
!                               types for the allocatable component; function
!                               return is a pointer array after bounds-remap.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, kind :: k
        integer, len :: dim

        real(k) :: x(dim)
    end type

    type base (k)
        integer, kind :: k

        type(point(k,:)), allocatable :: data(:,:)
    end type

    contains

    type(point(8,:)) function remapArray8 (p1, dim1, dim2)
        type(point(8,*)), intent(in), target :: p1(:)
        integer, intent(in) :: dim1, dim2

        pointer remapArray8(:,:)

        if (size(p1) < dim1*dim2) stop 20

        remapArray8(1:dim1,1:dim2) => p1
    end function
end module


program dtparamConstr054
use m
    type(point(8,:)), pointer :: p1(:)

    type(point(8,3)), target :: p2(100)

    type(base(8)), allocatable :: b1
    type(base(8)) :: b2

    logical(4), external :: precision_r8

    allocate (p1(100), source=(/(point(8,2)((/i, i+1/)), i=1,100)/))

    p2 = (/(point(8,3)((/sin(i*1.0d0), cos(i*1.0d0), tan(i*1.0d0)/)), i=1, 100)/)

    b2 = base(8)(remapArray8(p2(::2), 6, 5))

    allocate (b1, source=base(8)(remapArray8(p1, 10, 10)))

    if ((.not. allocated(b1%data)) .or. (.not. allocated(b2%data))) &
            error stop 1_4

    if ((b1%data%dim /= 2) .or. (b2%data%dim /= 3)) error stop 2_4

    if ((size(b1%data) /= 100) .or. (size(b2%data) /= 30)) error stop 3_4

    if (any(lbound(b1%data) /= 1) .or. any(ubound(b1%data) /= 10)) &
        error stop 4_4
        
    if (any(lbound(b2%data) /= (/1,1/)) .or. &
        any(ubound(b2%data) /= (/6,5/))) error stop 5_4

    k = 1

    do j = 1, 10
        do i = 1, 10
            if (.not. precision_r8(b1%data(i,j)%x(1), k*1.0d0)) error stop 6_4

            if (.not. precision_r8(b1%data(i,j)%x(2), (k+1)*1.0d0)) error stop 7_4

            k = k + 1
        end do
    end do

    k = 1

    do j = 1, 5
        do i = 1, 6
            if (.not. precision_r8(b2%data(i,j)%x(1), sin(k*1.0d0))) &
                error stop 8_4

            if (.not. precision_r8(b2%data(i,j)%x(2), cos(k*1.0d0))) &
                error stop 9_4

            if (.not. precision_r8(b2%data(i,j)%x(3), tan(k*1.0d0))) &
                error stop 10_4

            k = k + 2
        end do
    end do

    deallocate (p1, b1, b2%data)
end
