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
!*                               derived type without attribute.
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

    type, extends(base) :: child (kchild) ! kchild=1
       integer, kind :: kchild
    contains
       final :: finalizeChild
    end type

    type(child(4,1)), allocatable :: t_c1 ! tcx: (4,1)

    contains
    subroutine finalizeBase (b1)
        type (base(4)), intent(in) :: b1 ! tcx: (4)
        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (b1)
        type (child(4,1)), intent(in) :: b1 ! tcx: (4,1)
        print *, 'finalizeChild'
    end subroutine

end module

    use m

    call example
end

subroutine example

use m
    type(base(4)) :: dt1 ! tcx: (4)
    type(child(4,1)) :: dt2 ! tcx: (4,1)

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (kchild) to invoke with (4,1) / declare with (4,1) - 3 changes