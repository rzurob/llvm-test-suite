! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/17/2005
!*
!*  DESCRIPTION                : miscellaeous items
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc015a2
    type base
        integer(8) i1
        real, allocatable :: r1
        integer, pointer :: i2
    end type

    type (base) b1

    b1=base (100, 1.3, null())

    associate (x => transfer(b1, b1, 1))
        x%i1 = 10                       !<-- illegal

        deallocate (x(1)%r1)            !<-- illegal
        allocate (x(1)%i2, source=100)  !<-- illegal

    end associate
end
