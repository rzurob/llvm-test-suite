! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal529a2kl
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal529a2 by Jim Xia)
!*  DATE                       : 2007-11-11 (original: 06/18/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (allocated allocatable as the
!                               subobject of the actual-arg to be associated
!                               with INTENT(OUT) dummy-arg)
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

    type, extends (base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        final :: finalizeChild
    end type

    type container (kcontainer_1) ! kcontainer_1=4
       integer, kind :: kcontainer_1
        class (base(kcontainer_1)), allocatable :: data ! tcx: (kcontainer_1)
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

    subroutine test1 (co)
        type (container(4)), intent(OUT) :: co(:) ! tcx: (4)
    end subroutine

    subroutine test2 (co)
        class (container(4)), intent(OUT) :: co(:) ! tcx: (4)
    end subroutine
end module

program ffinal529a2kl
use m
    type (container(4)) :: co1(2) ! tcx: (4)

    allocate (co1(1)%data, co1(2)%data)

    call test1 (co1)

    if ((allocated (co1(1)%data)) .or. allocated (co1(2)%data)) error stop 101_4

    print *, 'test2'

    allocate (child(4,20) :: co1(1)%data, co1(2)%data) ! tcx: (4,20)

    call test2 (co1)

    if ((allocated (co1(1)%data)) .or. allocated (co1(2)%data)) error stop 2_4

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 2 changes
! type: container - added parameters (kcontainer_1) to invoke with (4) / declare with (4) - 3 changes
