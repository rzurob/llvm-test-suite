! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514a23klk
!*
!*  DATE                       : 2007-11-07 (original: 06/24/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (allocatable component
!                               autodeallocated in intrinsic assignment)
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

        contains

        final :: finalizeBase
        procedure :: print => printBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name = 'no-name'

        contains

        final :: finalizeChild
        procedure :: print => printChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b ! tcx: (4,*)

        print *, b%id, b%name
    end subroutine
end module

module m1
use m
    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        class (base(kdataType_1)), allocatable :: data ! tcx: (kdataType_1)

        contains

        procedure :: print => printData
    end type

    interface makeData
        function createData (i, c)
            import dataType
            type (dataType(4)) createData ! tcx: (4)
            integer*4, intent(in) :: i
            character(*), intent(in), optional :: c
        end function
    end interface

    contains

    subroutine printData (d)
        class (dataType(4)), intent(in) :: d ! tcx: (4)

        if (allocated (d%data)) call d%data%print
    end subroutine
end module

program ffinal514a23klk
use m1
    type (dataType(4)) :: d1 ! tcx: (4)

    d1 = makeData (10, 'test')

    call d1%print

    ! assignment again
    d1 = dataType(4) (data = child(4,20)()) ! tcx: (4,20) ! tcx: (4)

    call d1%print
end

function createData (i, c)
    use m1, only: dataType, child, base
    type (dataType(4)) createData ! tcx: (4)
    integer*4, intent(in) :: i
    character(*), intent(in), optional :: c

    type (child(4,20)), save :: c_static ! tcx: (4,20)

    if (present (c)) then
        c_static%id = i
        c_static%name = c
        allocate (createData%data, source=c_static)
    else
        allocate (createData%data)
        createData%data%id = i
    end if
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 4 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 5 changes
