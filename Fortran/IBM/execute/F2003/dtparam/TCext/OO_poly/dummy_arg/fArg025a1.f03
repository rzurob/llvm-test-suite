! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg025a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (declaration-type-spec as
!                               proc-interface in the
!                               procedure-declaration-stmt; the interface is
!                               implicit and has to allowable)
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
        integer(k1)   :: id = -1

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program fArg025a1
use m
    type (base(4)) :: b1 = base(4) (100)

    procedure (type (base(4))) :: makeData

    print *, makeData (base(4)(10))

    print *, makeData (b1)

    print *, 'end'
end

type (base(4)) function makeData (b)
use m
    type (base(4)), intent(in) :: b

    makeData%id = b%id
end