!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2005
!*
!*  DESCRIPTION                : specific type bound (use only the derived type
!                               should make binding name accessible, but not the
!                               sub-program name directly, i.e. the module
!                               procedure name is not a global symbol)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        contains

        procedure, nopass :: print => printBase
    end type

    type, extends(base) :: child
        integer*4, pointer :: id => null()
    end type

    contains

    subroutine printBase
        print *, 'base'
    end subroutine printBase

end module

program fspec500d
use m, only : child

    type (child) :: c1

    call c1%print     !<-- compiler should be OK

    call printBase     !<-- should NOT be accessible
end
