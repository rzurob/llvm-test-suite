! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (type-bound proc binding name
!*                               collides with data component)
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
        contains
        procedure, nopass :: value => baseValue
    end type

    type, extends (base) :: child
        real*4 :: value     !<-- illegal
    end type

    contains

        integer*4 function baseValue()
            baseValue = 0
        end function
end module

program fext039d
end