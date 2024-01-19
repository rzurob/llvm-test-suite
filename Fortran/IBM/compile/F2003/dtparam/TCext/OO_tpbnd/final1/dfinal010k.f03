!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-13 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing final subroutines:
!*                               Final subroutines shall not be a
!*                               function.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: x
        contains

!* expect error message 1514-598 here
        final  :: finalizeBase
    end type

    contains
    integer function finalizeBase (b1)
       type(base(4)), intent(out) :: b1  ! tcx: (4)
       finalizeBase = 10
    end function
end module
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
