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
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Expression is of the same rank for the
!                               allocatable components.
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
        integer(k/4) :: id
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: name
        logical(k) :: flag
    end type

    type container (k)
        integer, kind :: k

        class(base(k,:)), allocatable :: data(:)
    end type
end module

program dtparamConstr023
use m
    type (child(8,30,33)) c1(2,2)

    class(base(8,:)), allocatable :: c2(:,:)

    type(container(8)) co1

    logical(4), external :: precision_r8

    allocate(child(8,30,33) :: c2(0:1,-1:0))

    c2(1,-1)%data = exp(sqrt((/(i*1.0d0, i=1,30)/)))
    c2(1,0)%data = exp(sqrt(6.0d1))

    c2(1,:)%id = (/-120, 120/)


    co1 = container(8)(c2(1,:))

    if ((lbound(co1%data, 1) /= 1) .or. (ubound(co1%data, 1) /= 2)) &
            error stop 1_4

    select type (x => co1%data)
        type is (child(8,*,*))
            if (x%l /= 33) error stop 2_4
            if (x%n /= 30) error stop 3_4

            x%name = (/'c1(2,1) in main', 'c1(2,2) in main'/)
            x%flag = x%name == maxVal(x%name)

            c1(2,:) = x

        class default
            error stop 4_4
    end select


    !! verify c1(2,:)
    if ((c1(2,1)%id /= -120) .or. (c1(2,2)%id /= 120)) error stop 5_4

    do i = 1, 30
        if (.not. precision_r8(c1(2,1)%data(i), exp(sqrt(i*1.0d0)))) &
                error stop 5_4

        if (.not. precision_r8(c1(2,2)%data(i), exp(sqrt(6.0d1)))) &
                error stop 6_4
    end do

    if ((c1(2,1)%name /= 'c1(2,1) in main') .or. &
        (c1(2,2)%name /= 'c1(2,2) in main')) error stop 7_4


    if (c1(2,1)%flag .or. (.not. c1(2,2)%flag)) error stop 8_4
end
