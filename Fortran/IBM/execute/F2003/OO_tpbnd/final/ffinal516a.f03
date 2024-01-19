! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/16/2005
!*
!*  DESCRIPTION                : final sub (finalization of structure
!                               constructors used in specification expression)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) id

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal516a
use m
    integer(8) i1(size((/base(1_8), base(2_8)/)))

    if (size(i1) /= 2) error stop 1_4

    print *, 'end'
end
