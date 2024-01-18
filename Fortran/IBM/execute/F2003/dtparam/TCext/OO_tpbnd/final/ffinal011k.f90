! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal011k
!*
!*  DATE                       : 2007-10-31 (original: 02/10/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
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

    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        private
        integer(kbase_1) :: id

        contains
        private

        FINAL :: finalizeBase
    end type

    type (base(4)), pointer :: b1_m ! tcx: (4)
    type (base(4)), allocatable :: b2_m ! tcx: (4)

    type (base(4)), parameter :: b3_m = base(4) (1) ! tcx: (4) ! tcx: (4)

    public b1_m, b2_m, b3_m

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

end module

program ffinal011k
use m

    allocate (b1_m, b2_m)

    b1_m = b3_m
    b2_m = b3_m

    print *, 'before deallocate'

    deallocate (b1_m, b2_m)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
