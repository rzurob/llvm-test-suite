! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/19/2004
!*
!*  DESCRIPTION                : structure constructor (for allocatable
!                               components, ranks must be the same between
!                               expr and component)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fconstr032d2
    type base
        integer(8), allocatable :: id(:)
    end type

    type base1
        real(8), allocatable :: data
    end type

    type (base) b1
    type (base1) b11

    b1 = base (10)                      !<-- illegal

    b11 = base1 ((/1.0_8, 2.0_8/))      !<-- illegal
end
