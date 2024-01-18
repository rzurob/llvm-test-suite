! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514a7k
!*
!*  DATE                       : 2007-11-07 (original: 04/15/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of temps created by
!*                               function call in a pointer assignment
!*                               statement)
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

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    function produceBasePointer (b)
        type (base(4)), pointer :: produceBasePointer ! tcx: (4)
        type (base(4)), intent(in) :: b ! tcx: (4)

        type (base(4)), save, target :: temp ! tcx: (4)

        temp%id = b%id

        produceBasePointer => temp

        print *, 'returning from produceBasePointer'
    end function

    type (base(4)) function produceBaseObj (i) ! tcx: (4)
        integer*4, intent(in) :: i

        produceBaseObj%id = i
        print *, 'returning from produceBaseObj'
    end function
end module

program ffinal514a7k
use m
    type (base(4)), pointer :: b_ptr ! tcx: (4)

    b_ptr => produceBasePointer (produceBaseObj(10))

    if (b_ptr%id /= 10) error stop 101_4
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
