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
!                               Case: In the absence of the component keyword,
!                               the component data source is assigned according
!                               to component order; multi-generation derived
!                               type.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: base (k)
        integer, kind :: k

        integer(k) :: id
    end type

    type, extends(base) :: child(n)
        integer, len :: n

        real(2*k) :: data(n)
        logical(k/2) :: flag
    end type

    type, extends(child) :: gen3 (l)
        integer, len :: l

        character(l) :: name
        procedure(real(2*k)), nopass, pointer :: p1
    end type

    type, extends(gen3) :: gen4 (ck, m)
        integer, kind :: ck
        integer, len :: m

        complex(ck) :: cx(m)
    end type

    type (gen4(4, 10, 20, 8, 12)) g4_m

    procedure(double precision) calc1
end module

program dtparamConstr024
use m
    logical(4), external :: precision_r8, precision_r6

    g4_m = gen4(4, 10, 20, 8, 12) (100, exp(1.0d2), 10<100_1, &
        'g4_m of gen4 type in main', calc1, (/((i*1.0d0, 2.0d-1*i), i=1,12)/))


    !! verify results
    if (g4_m%id /= 100) error stop 1_4

    do i = 1, 10
        if (.not. precision_r8 (g4_m%data(i), exp(1.0d2))) error stop 2_4
    end do

    if (.not. g4_m%flag) error stop 3_4

    if (g4_m%name /= 'g4_m of gen4 type in') error stop 4_4

    if (.not. associated(g4_m%p1, calc1)) error stop 5_4

    do i = 1, 12
        if (.not. precision_r6(g4_m%cx(i), (i*1.0d0, 2.0d-1*i))) error stop 6_4
    end do

    if (.not. precision_r8(g4_m%p1(g4_m%cx), 6.6d1)) error stop 7_4
end


double precision function calc1 (cx)
    complex(8), intent(in) :: cx(10)

    calc1 = sum (abs(real(cx)))

    calc1 = calc1 + sum (abs(aimag(cx)))
end function
