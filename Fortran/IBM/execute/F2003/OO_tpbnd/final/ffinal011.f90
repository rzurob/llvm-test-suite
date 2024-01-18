!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/10/2005
!*
!*  DESCRIPTION                : final sub (final binding is accessible even if
!                               the type is not)
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
    private

    type base
        private
        integer*4 :: id

        contains
        private

        FINAL :: finalizeBase
    end type

    type (base), pointer :: b1_m
    type (base), allocatable :: b2_m

    type (base), parameter :: b3_m = base (1)

    public b1_m, b2_m, b3_m

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

end module

program ffinal011
use m

    allocate (b1_m, b2_m)

    b1_m = b3_m
    b2_m = b3_m

    print *, 'before deallocate'

    deallocate (b1_m, b2_m)

    print *, 'end'
end
