! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specific/ftpbnd520.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type-bound (binding needs to be
!*                               accessible in ASSOCIATE construct)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 1

        contains

        procedure :: print => printBase
        procedure :: addID => addID2Base
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine addID2Base (b, i)
        class (base(4)), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = b%id + i
    end subroutine
end module

program ftpbnd520
use m
    type (base(4)) :: b1

    associate (x => b1)
        call x%addID (9)

        call x%print
    end associate

    call b1%print
end
