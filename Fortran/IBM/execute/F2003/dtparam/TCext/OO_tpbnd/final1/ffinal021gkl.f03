!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-26 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing final subroutines:
!*                               derived type with pointer attribute.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: x
    contains
        final :: finalizeBase
    end type

    type, extends(base) :: child (lchild) ! lchild=23
       integer, len :: lchild
    contains
       final :: finalizeChild
    end type

    type(child(4,:)), allocatable :: t_c1 ! tcx: (4,:)

    contains
    subroutine finalizeBase (b1)
        type (base(4)), intent(in) :: b1 ! tcx: (4)
        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (b1)
        type (child(4,*)), intent(in) :: b1 ! tcx: (4,*)
        print *, 'finalizeChild'
    end subroutine

end module

    use m

    call example
end

subroutine example

use m
    type(base(4)), parameter :: dt1 = base(4)(2) ! tcx: (4) ! tcx: (4)
    type(child(4,23)),parameter :: dt2 = child(4,23)(3) ! tcx: (4,23) ! tcx: (4,23)

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (lchild) to invoke with (4,23) / declare with (4,*) - 4 changes
