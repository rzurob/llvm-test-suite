! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2005
!*
!*  DESCRIPTION                : structure constructor (DATA statement for
!                               derived type with nonpointer component default
!                               initialized)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    implicit type(child) (c)
    type base
        integer*4 :: id = 0
    end type

    type, extends(base) :: child
        character(20) :: name  = ''
    end type


!    type (child) :: c1_m, c2_m
    save c1_m, c2_m

    data c1_m / child(1, 'c1_m') /      !<-- illegal

    data c2_m%base / base(2) /          !<-- illegal
!    data c2_m%name / 'c2_m' /
!    data c2_m%id / 1 /
end module

program fconstr041d
end
