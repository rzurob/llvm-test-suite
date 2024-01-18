!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/25/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement(s): An extended type includes all of
!                               the type parameters of its parent.  Type
!                               parameters and components are class (2) local
!                               identifiers (can not have the two entities of
!                               the same name in the derived type definition)
!
!                               Case: the extended type contains a component
!                               with the same name as an inherited type
!                               parameter name.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, l, m)
        integer, kind :: k = 4
        integer, len :: l, m
        real(k) data(l:m)
    end type
end module

module m1
use m
    type, extends(base) :: child
        logical l           !<--    illegal
    end type

end module

program dtparamExtends010d
end
