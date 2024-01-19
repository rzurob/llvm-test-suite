! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (C1273: impure final binding cannot
!                               appear in a pure procedure)
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
        integer(4) :: id

        contains

        final :: finalizeBase
    end type

    type container
        class (base), pointer :: data

        contains

        final :: finalizeData
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    !! in pure procedure, all references to procedures, including finalization,
    !! should be pure
    pure subroutine finalizeData (d)
        type (container), intent(inout) :: d

        if (associated (d%data)) deallocate (d%data)  !<-- impure procedure call
    end subroutine
end module

program ffinal527d
end
