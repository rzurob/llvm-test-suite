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
!*  DATE                       : 02/27/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Components are supplied by parent
!                               components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data (n)
    end type

    type, extends(base) :: child(l)
        integer, len :: l

        character(l) :: name
    end type

    type, extends(child) :: gen3
        complex(k) :: cx(n)
    end type

    type (gen3(8,10,20)) :: g1 = gen3(8,10,20)(child=child(8,10,20) &
        ((/(i*1.0d0, i=1, 10)/), name='module var g1'), &
        cx = (/((i*1.0d0, i*1.0d0), i=1,10)/))
end module

program dtparamConstr013
use m
    class (child(8,:,:)), allocatable :: c1(:)

    logical(4), external :: precision_r8, precision_x6

    allocate (c1(10), source=(/(gen3(8,12,30)(cx=(/(cmplx(j+i*1.0d2, kind=8), j=1, 12)/),&
        child=child(8,12,30)(name='c1('//char(ichar('0')+i)//')', &
        base=base(8,12)((/(i*1.0d2+j, j=1, 12)/)))), i=0,9)/))


    !! verify results
    if (g1%name /= 'module var g1') error stop 1_4

    do i = 1, 10
        if (.not. precision_r8(i*1.0d0, g1%data(i))) error stop 2_4

        if (.not. precision_x6(g1%cx(i), (i*1.0d0, i*1.0d0))) error stop 3_4
    end do

    do i = 1, 10
        if (c1(i)%name /= 'c1('//char(ichar('0')+i-1)//')') error stop 4_4

        do j = 1, 12
            if (.not. precision_r8(c1(i)%data(j), (i-1)*1.0d2+j)) error stop 5_4
        end do
    end do

    select type (cx => c1)
        type is (gen3(8,*,*))
            do i = 1, 10
                do j = 1, 12
                    if (.not. precision_x6(cx(i)%cx(j), &
                        cmplx((i-1)*1.0d2+j, kind=8))) error stop 6_4
                end do
            end do

        class default
            error stop 7_4
    end select
end
