! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514a5k
!*
!*  DATE                       : 2007-10-12 (original: 04/13/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (temporaries created by function
!*                               calls shall be finalized; test in the forall
!*                               header in forall statement)
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

    pure subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        b%id = -1
    end subroutine
end module

module m1
use m, only : base
    type (base(4)) :: b1_m (10) ! tcx: (4)
end module

program ffinal514a5k
use m1
    interface operator (==)
        pure logical function baseEqual (b1, b2)
        use m
            type (base(4)), intent(in) :: b1, b2 ! tcx: (4)
        end function
    end interface

    interface
        pure type (base(4)) function produceBase (i) ! tcx: (4)
        use m
            integer*4, intent(in) :: i
        end function
    end interface

    type (base(4)), save :: b1(3) ! tcx: (4)

    data b1 /base(4)(1), base(4)(2), base(4)(3)/ ! tcx: (4) ! tcx: (4) ! tcx: (4)

    b1_m%id = (/(i, i=1,10)/)

    forall (i=1:10, b1_m(i) == produceBase(3))     b1_m(i)%id = -3

    forall (i=1:3, b1(i) == produceBase(i))        b1(i)%id = -i

    print *, b1_m
    print *, b1
end


pure logical function baseEqual (b1, b2)
use m
    type (base(4)), intent(in) :: b1, b2 ! tcx: (4)

    baseEqual = (b1%id == b2%id)
end function


pure type (base(4)) function produceBase (i) ! tcx: (4)
use m
    integer*4, intent(in) :: i

    produceBase%id = i
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 10 changes
