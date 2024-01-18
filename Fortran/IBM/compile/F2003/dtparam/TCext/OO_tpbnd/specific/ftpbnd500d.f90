! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd500d.f
! opt variations: -qnok -qnol -qreuse=none

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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: print => printBase
    end type

    type, extends(base) :: child    ! (4,20)
        integer(k1), pointer :: id => null()
    end type

    contains

    subroutine printBase
        print *, 'base'
    end subroutine printBase

end module

program fspec500d
use m, only : child

    type (child(4,20)) :: c1

    call c1%print     !<-- compiler should be OK

    call printBase     !<-- should NOT be accessible
end
