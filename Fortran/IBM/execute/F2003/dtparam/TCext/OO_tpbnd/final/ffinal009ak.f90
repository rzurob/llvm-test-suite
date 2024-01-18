! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal009ak
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal009a by Jim Xia)
!*  DATE                       : 2007-10-31 (original: 02/09/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (pointer function return containing
!                               allocatable array components)
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
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        real(kbase_1), allocatable :: data(:)

        contains

        final :: finalizeBase, finalizeBaseArray1
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1), allocatable :: name

        contains

        final :: finalizeChild, finalizeChildArray1
    end type

    type dataType (kdataType_1) ! kdataType_1=8
       integer, kind :: kdataType_1
        class (base(kdataType_1)), allocatable :: data (:) ! tcx: (kdataType_1)

        contains

        procedure, nopass :: genData => genDataTypePtr
    end type

    contains

    subroutine finalizeBase (b)
        type (base(8)), intent(in) :: b ! tcx: (8)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base(8)), intent(in) :: b(:) ! tcx: (8)

        print *, 'finalizeBaseArray1'
    end subroutine

    subroutine finalizeChild (c)
        type (child(8,*)), intent(in) :: c ! tcx: (8,*)

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildArray1 (c)
        type (child(8,*)), intent(in) :: c(:) ! tcx: (8,*)

        print *, 'finalizeChildArray1'
    end subroutine

    class (dataType(8)) function genDataTypePtr (data, name, isize, csize) ! tcx: (8)
        pointer genDataTypePtr(:)

        real(8), intent(in) :: data(:)
        character(*), intent(in), optional :: name
        integer, intent(in) :: isize, csize

        allocate (genDataTypePtr(isize))

        if (present(name)) then
            do i = 1, isize
                allocate (genDataTypePtr(i)%data(csize), &
                            source=child(8,20)(data, name)) ! tcx: (8,20)
            end do
        else
            do i = 1, isize
                allocate (genDataTypePtr(i)%data(csize), source=base(8)(data)) ! tcx: (8)
            end do
        end if
    end function
end module


program ffinal009ak
use m
    class (dataType(8)), pointer :: d1(:) ! tcx: (8)

    nullify (d1)

    d1 => d1%genData ((/1.0_8, 2.0_8/), isize=3, csize=2)

    print *, 'test 1'

    deallocate (d1)

    print *, 'test 2'

    d1 => d1%genData ((/1.0_8, 2.0_8/), 'abc', 2, 3)

    deallocate (d1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (8,20) / declare with (8,*) - 3 changes
! type: dataType - added parameters (kdataType_1) to invoke with (8) / declare with (8) - 2 changes
