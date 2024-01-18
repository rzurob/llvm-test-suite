! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal515a5k
!*
!*  DATE                       : 2007-11-11 (original: 04/23/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (structure constructor created temps
!*                               finalized; IF-stmt)
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: id

        contains

        final :: finalizeBase
    end type

    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        class (base(kdataType_1)), pointer :: data => null() ! tcx: (kdataType_1)

        contains

        final :: finalizeData
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeData (d)
        type (dataType(4)), intent(inout) :: d ! tcx: (4)

        print *, 'finalizeData'

        if (associated (d%data)) then
            print *, 'deallocating d%data'
            deallocate (d%data)
        end if
    end subroutine

    logical function isAssociated (d)
        class (dataType(4)), intent(in) :: d ! tcx: (4)

        isAssociated = associated (d%data)
    end function
end module

program ffinal515a5k
use m
    type (base(4)), pointer :: b1 ! tcx: (4)

    allocate (b1)
    b1%id = 10

    if (isAssociated (dataType(4)(data = b1)))     print *, 'true' ! tcx: (4)

    print *, 'false condition test'

    if (isAssociated (dataType(4)())) error stop 101_4 ! tcx: (4)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 4 changes
