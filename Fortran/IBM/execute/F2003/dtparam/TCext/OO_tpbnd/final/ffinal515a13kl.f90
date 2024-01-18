! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal515a13kl
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal515a13 by Jim Xia)
!*  DATE                       : 2007-11-07 (original: 02/14/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (finalization of the temp created by
!                               the structure constructor in the pointer
!                               assignment statement)
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
        integer(kbase_1) id

        contains

        final :: finalizeBase
        procedure, nopass :: makeData => produceBasePtr
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=10
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base(8)), intent(in) :: b ! tcx: (8)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(8,*)), intent(in) :: c ! tcx: (8,*)

        print *, 'finalizeChild'
    end subroutine

    class (base(8)) function produceBasePtr (b) ! tcx: (8)
        pointer produceBasePtr
        class (base(8)), intent(in) :: b ! tcx: (8)

        allocate (produceBasePtr, source=b)
    end function
end module


program ffinal515a13kl
use m
    class (base(8)), pointer :: b1, b2 ! tcx: (8)

    nullify (b2)

    b1 => b2%makeData(b2%makeData(child(8,10) (1, 'test'))) ! tcx: (8,10)

    !! verify the results
    select type (b1)
        type is (child(8,*)) ! tcx: (8,*)
            print *, b1
        class default
            error stop 101_4
    end select
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (8,10) / declare with (8,*) - 3 changes
