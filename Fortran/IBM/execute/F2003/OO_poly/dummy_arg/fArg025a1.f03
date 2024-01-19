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
    type base
        integer*4 :: id = -1

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program fArg025a1
use m
    type (base) :: b1 = base (100)

    procedure (type (base)) :: makeData

    print *, makeData (base(10))

    print *, makeData (b1)

    print *, 'end'
end

type (base) function makeData (b)
use m
    type (base), intent(in) :: b

    makeData%id = b%id
end
