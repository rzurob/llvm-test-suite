! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514a23dklk
!*
!*  DATE                       : 2007-11-07
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                :
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
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name = 'no-name'

        contains

        final :: finalizeChild
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
end module

module m1
use m
    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        class (base(kdataType_1)), allocatable :: data ! tcx: (kdataType_1)
    end type

    interface makeData
        function createData (i, c)
            import dataType
            type (dataType(4)) createData ! tcx: (4)
            integer*4, intent(in) :: i
            character(*), intent(in), optional :: c
        end function
    end interface
end module

program ffinal514a23dklk
use m1
    print *, makeData (10, 'test')
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
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 2 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 2 changes
