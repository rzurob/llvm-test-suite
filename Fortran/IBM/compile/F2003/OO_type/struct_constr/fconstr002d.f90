! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : default init (C1107)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 ::id
        real*4 :: value = 1.0
    end type

    type, extends (base) :: child
        character (20) :: name
    end type

    type (base) :: b2_m = base (id = 1)
    type (base) :: b3_m = base (1, 10.0)

    type (child) :: c2_m = child (id = 2, value = 10.0, name='c2_m')
    type (child) :: c3_m = child (base = base (id = 3), name = 'c3_m')

    type (child) :: c1_m  !<-- this shall have SAVE attribute
end module

program fconstr002d
end
