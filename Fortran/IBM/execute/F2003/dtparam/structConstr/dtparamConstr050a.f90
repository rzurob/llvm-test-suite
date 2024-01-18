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
!*  DATE                       : 08/14/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               Use of derived type (no type parameters) for the
!                               component.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real :: x, y
    end type

    type base (n)
        integer, len :: n

        type(point) :: data(n)
    end type

    type container (n)
        integer, len  :: n

        class(base(n)), allocatable :: data(:)
    end type
end module

program dtparamConstr050a
use m
    type (container(:)), allocatable :: co1

    type(base(:)), allocatable :: b1(:)

    logical(4), external :: precision_r4

    allocate(b1(0:7), source = &
            (/(base(5)(data=(/(point(x=j,y=j), j=i-4, i)/)), i=1, 8)/))


    co1 = container(5)(data=b1)

    deallocate (b1)

    if (.not. allocated(co1)) error stop 1_4

    if (.not. allocated(co1%data)) error stop 2_4

    if ((lbound(co1%data,1) /= 0) .or. (ubound(co1%data,1) /= 7)) error stop 3_4

    do i = 0, 7
        do j = 1, 5
            if (.not. precision_r4 (co1%data(i)%data(j)%x, (i+j-4)*1.0_4)) &
                error stop 4_4

            if (.not. precision_r4 (co1%data(i)%data(j)%y, (i+j-4)*1.0_4)) &
                error stop 5_4
        end do
    end do
end
