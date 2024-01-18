! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-01 (original: 02/10/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (no finalization will occurr due to
!                               the execution termination of program by error
!                               condition)
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), pointer :: data(:) => null()

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase(b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'

        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
        end if
    end subroutine
end module

program ffinal506k
    call test1
end

subroutine test1
use m
    type (base(4)), allocatable, target :: b1 ! tcx: (4)
    class (base(4)), allocatable :: b2 ! tcx: (4)
    class (base(4)), pointer :: b3 ! tcx: (4)

    type (base(4)) b4 ! tcx: (4)

    allocate (b1, b2)

    allocate (b1%data(20))

    b3 => b1

    print *, 'STDOUT ends here'

    deallocate (b3)     !<-- execution get terminated here

    !! neither deallocation of b1, b2 nor finalization of b4 will occurr
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
