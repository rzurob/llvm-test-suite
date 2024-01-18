!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/28/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: diagnostic test case.  The type parameters
!                               can not be specified with attributes that are
!                               allowed only for components (pointer,
!                               allocatable, dimension and private/public).  In
!                               other words, type parameters can not treated the
!                               same as components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n, o)
        integer, len, private :: n          !<--illegal
        integer, public, len :: o           !<--illegal
    end type
end module

program dtparamExtends011d2
    type base
    end type

    type, extends(base) :: child(k, l, m)
        integer, kind, allocatable :: k     !<--illegal
        integer, pointer, len :: l          !<--illegal
        integer, dimension(3), len :: m     !<--illegal
    end type
end
