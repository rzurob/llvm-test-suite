! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr032d2.f
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
    type base(k1)    ! (8)
        integer, kind            :: k1
        integer(k1), allocatable :: id(:)
    end type

    type base1(k2)    ! (8)
        integer, kind         :: k2
        real(k2), allocatable :: data
    end type

    type (base(8)) b1
    type (base1(8)) b11

    b1 = base(8) (10)                      !<-- illegal

    b11 = base1(8) ((/1.0_8, 2.0_8/))      !<-- illegal
end
