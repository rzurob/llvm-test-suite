! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd520a.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (PROTECTED variable shall
!                               be allowed to be used as the selector in the
!                               ASSOCIATE construct)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id = 1

        contains

        procedure :: print => printBase
        procedure :: addID => addID2Base
    end type

    type (base(20,4)), protected :: b1_m = base(20,4) (10)

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine addID2Base (b, i)
        class (base(*,4)), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = b%id + i
    end subroutine

    subroutine assignB1_m (id)
        integer*4, intent(in) :: id
        b1_m%id = id
    end subroutine
end module

program ftpbnd520a
use m
    associate (x => b1_m)
        call x%print

        call assignB1_m (100)

        call x%print
    end associate
end
