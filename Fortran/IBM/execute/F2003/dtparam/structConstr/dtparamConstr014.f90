!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/28/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Components are supplied by parent
!                               component default initialization.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n) = -1.0
    end type
end module

module m1
use m
    type, extends(base) :: child (l)
        integer, len :: l

        integer(k) :: id(n) = -k
        character(l) :: name
    end type

    type(child(4, 100, 20)) :: c1 = child(4, 100, 20)&
        (base=base(4, 100)(), name='module var c1')
end module

module m2
use m1
    type, extends(child) :: gen3
        complex(k) :: cx(n*2) = cmplx(k, 2*k, kind=k)
    end type
end module

program dtparamConstr014
use m2
    type (gen3(8,:,:)), allocatable :: g1

    logical(4), external :: precision_r4, precision_r8, precision_x6

    allocate (gen3(8,120,30) :: g1)

    g1 = gen3(8,120,30)(child=child(8,120,30)(name='variable in main program'))

    !! verify c1 and g1
    do i = 1, 100
        if (.not. precision_r4(c1%data(i), -1.0)) error stop 1_4

        if (c1%id(i) /= -4) error stop 2_4
    end do

    if (c1%name /= 'module var c1') error stop 3_4


    do i = 1, 120
        if (.not. precision_r8(g1%child%base%data(i), real(-1.0, 8))) &
            error stop 4_4

        if (g1%child%id(i) /= -8) error stop 5_4

        if (.not. precision_x6(g1%cx(2*i-1), cmplx(8,16,kind=8))) error stop 6_4
    end do

    if (g1%name /= 'variable in main program') error stop 7_4
end
