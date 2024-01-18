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
!*  DATE                       : 08/11/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               Use expression (array arithematics) as the data
!                               source for allocatable components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        integer(min(k,8)), allocatable :: ids(:,:)
        real(k), allocatable :: data(:)
    end type
end module

program dtparamConstr049a1
use m
    integer, target :: i1(100)
    integer, pointer :: i2(:,:)
    integer, allocatable :: i3(:,:)

    integer, parameter :: intKind = 4

    real(8), allocatable :: d1(:)

    type(base(8)), allocatable :: b1

    logical(4), external :: precision_r8, precision_r4

    class(base(intKind)), pointer :: b2(:)

    allocate (d1(0:19))
    allocate (i3(0:2, 3), source=reshape((/(i, i=1,10)/), (/3,3/)))

    d1 = (/(i, i=1, 20)/)

    i1 = (/(i/3+1, i=1,100)/)

    i2(2:4, 0:2) => i1(::3)

    b1 = base(8)(i2*i3, d1+1)

    if ((.not. allocated(b1%ids)) .or. (.not. allocated(b1%data))) &
            error stop 1_4

    if (any(lbound(b1%ids) /= 1) .or. any(ubound(b1%ids) /= 3)) error stop 2_4

    if ((lbound(b1%data,1) /= 1) .or. (ubound(b1%data,1) /= 20)) error stop 3_4

    if (any((/b1%ids/) /= (/(i**2, i=1,9)/))) error stop 4_4

    do i = 1, 20
        if (.not. precision_r8(b1%data(i), (i+1)*1.0d0)) error stop 5_4
    end do


    !! test 2 for b2
    allocate (b2(2), source=base(4)(cshift(i2,-1, 2), pack(i2, i2>3)))

    if ((.not. allocated(b2(1)%ids)) .or. (.not. allocated(b2(1)%data))) &
            error stop 6_4

    if ((.not. allocated(b2(2)%ids)) .or. (.not. allocated(b2(2)%data))) &
            error stop 7_4

    if (any ((/b2(1)%ids/) /= (/7,8,9,1,2,3,4,5,6/))) error stop 8_4

    if ((b2(2)%ids(1,1) /= 7) .or. (b2(2)%ids(2,1) /= 8) .or. &
        (b2(2)%ids(3,1) /= 9)) error stop 9_4

    if (any(b2(2)%ids(:,2) /= (/1,2,3/))) error stop 10_4

    if (any(b2(2)%ids(:,3) /= (/4,5,6/))) error stop 11_4

    do i = 1, 6
        if (.not. precision_r4(b2(1)%data(i), (i+3)*1.0)) error stop 12_4

        if (.not. precision_r4(b2(2)%data(i), (i+3)*1.0)) error stop 13_4
    end do
end
