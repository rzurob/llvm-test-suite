!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-12 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing final subroutines: The dummy
!*                               argument shall not be intent(out)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: x
        contains
        final :: finalizeBase !* expect an error message here
    end type

    type, extends(base) :: child
    contains
       final :: finalizeChild !* expect an error message here
    end type

    contains
    subroutine finalizeBase (b1)
       type(base(4)), intent(out) :: b1 ! tcx: (4)
       print *, 'finalizeBase'
    end subroutine
    subroutine finalizeChild (b1)
       type(child(4)), intent(out) :: b1 ! tcx: (4)
       print *, 'finalizeChild'
    end subroutine
end module
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 1 changes