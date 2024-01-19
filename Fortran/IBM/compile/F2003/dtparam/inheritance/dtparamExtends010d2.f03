! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/25/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statements: Additional type parameters may be
!                               declared in the definition of the extended type.
!                               Ttype parameters and components are class (2)
!                               local identifiers (can not have the two entities
!                               of the same name in the derived type definition)
!                               Case: the extended type contains new type
!                               parameters with the same name as the accessible
!                               component from its parent.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamExtends010d2
end

module m
    type base
        real(4) k
        integer(4), private :: l(10) = -1
    end type
end module

module m1
use m
    type, extends(base) :: child (k,l)
        integer, kind :: k = 4              !<-- illegal
        integer, len :: l = 10              !<-- this is OK

        real(k) data (l)
    end type
end module

