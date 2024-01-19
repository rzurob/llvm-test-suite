! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/01/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: correct usage of structure constructors
!                               for the case dtparamConstr015d
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: name
    end type
end module

program dtparamConstr015
use m
    type (child(8, 10, 12)) :: c1
    type(child(4, :,:)), allocatable :: c2

    logical(4), external :: precision_r4, precision_r8

    allocate (child(4,200, 20) :: c2)

    c1 = child(8, 10, 12)(data=1.0d0, name='xlftest')

    c2 = child(4,200,20)(base=base(4,200)((/(i*1.2,i=1,200)/)), &
                name='xlftest again')

    !! verify
    if ((c1%name /= 'xlftest') .or. (c2%name /= 'xlftest again')) &
        error stop 1_4

    do i = 1, 10
        if (.not. precision_r8(1.0d0, c1%base%data(i))) error stop 2_4
    end do

    do i = 1, 200
        if (.not. precision_r4(c2%data(i), i*1.2)) error stop 3_4
    end do
end
