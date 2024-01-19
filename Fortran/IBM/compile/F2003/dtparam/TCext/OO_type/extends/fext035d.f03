! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (empty base type and name
!*                               conflict of inherited component)
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
    type base(k1)
        integer, kind :: k1
    end type

    type, extends(base) :: child(k2)
        integer, kind :: k2
        integer(k2) :: i2
    end type

    type, extends(child) :: thirdGeneration(k3)
        integer, kind :: k3
        real(k3) :: i2        !<-- this is name conflict
    end type

end module

program fext035d

end

