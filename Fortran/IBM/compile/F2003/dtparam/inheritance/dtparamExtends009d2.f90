!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/24/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement(s): An extended type includes all of
!                               the type parameters of its parent.   type
!                               parameters are class (2) local identifiers (can
!                               not have the two entities of the same name in
!                               the derived type definition)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k,l)
        integer, kind :: k = 4
        integer(k), len :: l = 10

        private
        real(k) :: data (l) = 0.0
        integer(k) :: id = -1
    end type

    type, extends(base) :: child
        private
        character(l) :: name = 'default'
    end type
end module

module m1
use m
    type, extends(child) :: gen3 (k,m)
        integer, kind :: k      !<-- illegal
        integer, len :: m

        real(k) data(l,m)       !<-- this is legal
    end type
end module

program dtparamExtends009d2
end
