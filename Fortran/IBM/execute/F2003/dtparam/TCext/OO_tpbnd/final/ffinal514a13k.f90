! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514a13k
!*
!*  DATE                       : 2007-11-07 (original: 04/19/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of function created
!*                               temps in READ and WRITE statement after use)
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
        integer(kbase_1) ::id = 1

        contains

        final :: finalizeBase, finalizeBaseRank1

        procedure :: makeArray => makeArrayFromBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine

    type (base(4)) function makeArrayFromBase (b, n) ! tcx: (4)
        class (base(4)), intent(in) :: b ! tcx: (4)
        integer*4, intent(in) ::  n
        dimension makeArrayFromBase(n)

        makeArrayFromBase%id = b%id
    end function
end module

program ffinal514a13k
use m
    type (base(4)) :: b1(3), b2(1) ! tcx: (4)


    do i = 1, 3
        read (*,*) b1(size(b1(i)%makeArray(i)))
    end do

    do i = 2, 4
        write (*,*) b1(size(b1(i-1)%makeArray(i-1)))
    end do
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
