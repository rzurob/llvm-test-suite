! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal501a2_1kl
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal501a2_1 by Jim Xia)
!*  DATE                       : 2007-11-01 (original: 02/10/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (finalization of allcoatable
!                               components during intrinsic assignment)
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

        final :: finalizeBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=3
       integer, len :: lchild_1
        character(lchild_1), allocatable :: name(:)

        contains

        final :: finalizeChild, finalizeChildArray1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(8)), intent(inout) :: b ! tcx: (8)

        print *, 'finalizeBase'

        if (allocated (b%data)) deallocate (b%data)
    end subroutine

    subroutine finalizeChild (c)
        type (child(8,*)), intent(inout) :: c ! tcx: (8,*)

        print *, 'finalizeChild'

        if (allocated (c%name)) deallocate (c%name)

        call finalizeBase (c%base)
    end subroutine

    subroutine finalizeChildArray1 (c)
        type (child(8,*)), intent(inout) :: c(:) ! tcx: (8,*)

        print *, 'finalizeChildArray1'

        do i = 1, size (c)
            call finalizeChild (c(i))
        end do
    end subroutine
end module

program ffinal501a2_1kl
use m
    type dataType (kdataType_1) ! kdataType_1=8
       integer, kind :: kdataType_1
        class (base(kdataType_1)), allocatable :: data(:) ! tcx: (kdataType_1)
    end type

    type (dataType(8)) d1, d01 ! tcx: (8)


    allocate (d01%data(2), source=(/child(8,3)((/1.1_8, 2.1_8/), (/'abc', 'xyz'/)), & ! tcx: (8,3)
            child(8,3) ((/3.1_8, 4.0_8/), (/'ABC', 'XYZ'/))/)) ! tcx: (8,3)

    d1 = d01

    print *, 'test 1'

    d1 = d01

    select type (x => d1%data)
        type is (child(8,*)) ! tcx: (8,*)
            if (size(x) /= 2) error stop 2_4

            write (*, '(4(1x, f10.2))') x(1)%data, x(2)%data

            print *, x(1)%name, x(2)%name
        class default
            error stop 101_4
    end select
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 2 changes
! type: child - added parameters (lchild_1) to invoke with (8,3) / declare with (8,*) - 5 changes
! type: dataType - added parameters (kdataType_1) to invoke with (8) / declare with (8) - 1 changes
