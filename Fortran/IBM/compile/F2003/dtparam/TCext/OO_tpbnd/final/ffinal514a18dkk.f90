! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514a18dkk
!*
!*  DATE                       : 2007-11-07 (original: 06/11/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (derived types with allocatable or
!                               pointer component not allowed in IO, only DTIO)
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
        integer(kbase_1) :: id = -1
    end type

    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        class (base(kdataType_1)), pointer :: data => null() ! tcx: (kdataType_1)

        contains

        final :: finalizeData
    end type

    type (dataType(4)), save :: d1_m ! tcx: (4)

    contains

    subroutine finalizeData (d)
        type (dataType(4)), intent(inout) :: d ! tcx: (4)

        print *, 'finalizeData'

        if (associated (d%data)) then
            print *, 'deallocating d%data'
            deallocate (d%data)
        end if
    end subroutine
end module

module m1
use m, only : base
    type, extends (base) :: child (kchild_1) ! kchild_1=4
       integer, kind :: kchild_1
        integer(kchild_1), pointer :: value => null()

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child(4,4)), intent(inout) :: c ! tcx: (4,4)

        print *, 'finalizeChild'

        if (associated (c%value)) then
            print *, 'deallocating c%value'
            deallocate (c%value)
        end if
    end subroutine
end module

module m2
    interface makeData
        function makeDataOfBase (id)
        use m
            type (dataType(4)) makeDataOFBase ! tcx: (4)
            integer*4, intent(in) :: id
        end function

        function makeDataOfChild (id, value)
        use m
        use m1, only : child
            type (dataType(4)) makeDataOfChild ! tcx: (4)
            integer*4, intent(in) :: id, value
        end function
    end interface
end module

program ffinal514a18dkk
use m
use m1, only: child
use m2

    print *, makeData(10)
    print *, makeData (10, 10)
end



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (kchild_1) to invoke with (4,4) / declare with (4,4) - 1 changes
