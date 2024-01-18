!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal021hkk.f
!*  TEST CASE NAME             : type-bound procedure ffinal021hkk
!*
!*  DATE                       : 2007-11-26 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing final subroutines:
!*                               derived type with allocatable attribute.
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

    type, extends(base) :: child (kchild) ! kchild=128
       integer, kind :: kchild
    contains
       final :: finalizeChild
    end type

    type(child(4,128)), allocatable :: t_c1 ! tcx: (4,128)

    contains
    subroutine finalizeBase (b1)
        type (base(4)), intent(in) :: b1 ! tcx: (4)
        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (b1)
        type (child(4,128)), intent(in) :: b1 ! tcx: (4,128)
        print *, 'finalizeChild'
    end subroutine

end module

    use m

    call example
end

subroutine example

use m
    type(base(4)), allocatable :: dt1  ! tcx: (4)
    type(child(4,128)),allocatable :: dt2  ! tcx: (4,128)

    print *, "example"

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (kchild) to invoke with (4,128) / declare with (4,128) - 3 changes
