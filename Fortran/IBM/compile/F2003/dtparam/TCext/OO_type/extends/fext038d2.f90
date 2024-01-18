! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (componet name is same as
!*                               the type name, can't be extended)
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

    type, extends(base) :: child(n,k2)
        integer, len :: n
        integer, kind :: k2
        character(n) :: name
        integer(k2) :: child
    end type

    type, extends(child) :: t(k3)   !<-- child can not be extended
        integer, kind :: k3
        logical(k3) :: isSet
    end type

end module

program fext038d2
end
