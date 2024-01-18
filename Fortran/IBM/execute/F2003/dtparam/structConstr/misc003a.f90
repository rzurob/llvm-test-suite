!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/28/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 316767)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base! (k, n)
        real(4) :: data(100) = -1.0
    end type
end module

module m1
use m
    type, extends(base) :: child! (l)
        integer(4) :: id(100) = -4
        character(20) :: name
    end type

    type(child), save :: c1 = child (base=base(), name='module var c1')
end module


program dtparamConstr014
use m1
    logical(4), external :: precision_r4

    !! verify c1 and g1
    do i = 1, 100
        if (.not. precision_r4(c1%data(i), -1.0)) error stop 1_4

        if (c1%id(i) /= -4) error stop 2_4
    end do

    if (c1%name /= 'module var c1') error stop 3_4
end
