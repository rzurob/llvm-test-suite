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
!*  DATE                       : 02/24/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Expression used in the component data
!                               source; use intrinsic type for allocatable
!                               component.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, l)
        integer, kind :: k
        integer, len :: l

        real(k), allocatable :: data(:,:)
        character(l) :: name
    end type
end module

program dtparamConstr009
use m
    type (base(8,:)), allocatable :: b1(:)
    type (base(4,20)) :: b2, b3(5)

    logical(4), external :: precision_r4, precision_r8

    character(20) :: names1(5), names2(10)
    real(8) :: d
    real(4) :: r1

    b2 = base(4,20)(null(), 'b2 in main')

    b3 = (/base(4,20) :: (base(4,20)(reshape((/(i*1.0e0+j*1.0e1, i=1, 15)/), &
            (/3, 5/)), 'b3('//char(ichar('0')+j)//') in main'), j=1, 5)/)

    allocate (base(8, 25) :: b1(10))

    b1 = (/base(8,25) :: (base(8,25)(reshape((/(j*1.1d0+i*1.1d-1, i=1,20)/), &
            (/2, 10/)), 'b1('//char(ichar('0')+j)//')'), j=0, 9)/)

    names1 = (/'b3(1) in main', 'b3(2) in main', 'b3(3) in main', &
               'b3(4) in main', 'b3(5) in main'/)

    names2 = (/'b1(0)', 'b1(1)', 'b1(2)', 'b1(3)', 'b1(4)', 'b1(5)', &
               'b1(6)', 'b1(7)', 'b1(8)', 'b1(9)'/)

    !! verify b1, b2 and b3
    do i = 1, 10
        if (b1(i)%name /= names2(i)) error stop 1_4

        d = 1.1d-1

        do k = 1, 10
            do j = 1, 2
                if (.not. precision_r8(b1(i)%data(j, k), (i-1)*1.1d0 + d)) &
                        error stop 2_4

                d = d + 1.1d-1
            end do
        end do
    end do

    if (allocated (b2%data)) error stop 3_4

    do i = 1, 5
        r1 = 1.0e0

        do k = 1, 5
            do j = 1, 3
                if (.not. precision_r4(b3(i)%data(j, k), i*1.0e1+r1)) &
                        error stop 4_4

                r1 = r1 + 1.0e0
            end do
        end do
    end do
end
