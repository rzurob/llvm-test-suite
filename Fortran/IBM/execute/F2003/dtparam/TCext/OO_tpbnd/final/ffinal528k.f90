! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-11 (original: 04/26/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (nonpointer, nonallocatable local
!*                               variables finalized due to END statement)
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
        integer(kbase_1), pointer :: data(:) => null()

        contains

        final :: finalizeBase
    end type

    type, extends (base) :: child
        integer(kbase_1), pointer :: data2 => null()

        contains

        final :: finalizeChild
    end type

    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        class (base(kdataType_1)), pointer :: data => null() ! tcx: (kdataType_1)

        contains

        final :: finalizeData
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'

        if (associated (b%data))  deallocate (b%data)
    end subroutine

    subroutine finalizeChild (c)
        type (child(4)), intent(inout) :: c ! tcx: (4)

        print *, 'finalizeChild'

        if (associated (c%data2)) deallocate (c%data2)
    end subroutine

    subroutine finalizeData (d)
        type (dataType(4)), intent(inout) :: d ! tcx: (4)

        print *, 'finalizeData'

        if (associated (d%data)) deallocate (d%data)
    end subroutine
end module

program  ffinal528k
    call abc

    call cba
end

subroutine abc
use m
    type (dataType(4)), target :: d1 ! tcx: (4)
end subroutine

subroutine cba
use m
    type (dataType(4)) :: d1 ! tcx: (4)

    allocate (child(4) :: d1%data) ! tcx: (4)
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 2 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 3 changes
