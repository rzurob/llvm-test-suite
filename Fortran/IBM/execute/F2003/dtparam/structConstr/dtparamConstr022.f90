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
!*  DATE                       : 03/06/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Data-target is of the same rank from the
!                               pointer component.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k,n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        integer(k/2) :: id
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: name
        logical(k) :: flag
    end type

    type container (k)
        integer, kind :: k

        class(base(k,:)), pointer :: data(:)
    end type
end module

program dtparamConstr022d
use m
    type(child(8, 30, 31)), target :: c1(2,2)
    type(container(8)) :: co1

    logical(4), external :: precision_r8

    co1 = container(8)(data=c1(2,:))

    if (.not. associated(co1%data, c1(2,1:2))) error stop 1_4

    if (co1%data%n /= 30) error stop 2_4

    co1%data%id = (/-1, 2**25/)

    co1%data(1)%data = exp(sqrt(((/(i*1.0d0, i=1,30)/))))

    co1%data(2)%data = exp(sqrt(31*1.0d0))

    select type (x => co1%data)
        type is (child(8,*,*))
            if (x%l /= 31) error stop 3_4

            x%name = (/'c1(2,1) of child type', 'c1(2,2) of child type'/)
            x%flag = .true.

        class default
            error stop 4_4
    end select


    !! verify results
    do i = 1, 30
        if (.not. precision_r8(c1(2,1)%data(i), exp(sqrt(i*1.0d0)))) &
                error stop 5_4

        if (.not. precision_r8(c1(2,2)%data(i), exp(sqrt(31*1.0d0)))) &
                error stop 6_4
    end do

    if ((c1(2,1)%id /= -1) .or. (c1(2,2)%id /= 2**25)) error stop 7_4

    if ((c1(2,1)%name /= 'c1(2,1) of child type') .or. &
        (c1(2,2)%name /= 'c1(2,2) of child type')) error stop 8_4


    if ((.not. c1(2,1)%flag) .or. (.not. c1(2,2)%flag)) error stop 9_4
end
