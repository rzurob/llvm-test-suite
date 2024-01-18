! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal514a22k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal514a22 by Jim Xia)
!*  DATE                       : 2007-11-07 (original: 04/22/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (pointers returned from function
!*                               calls are not finalized)
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
        integer(kbase_1) :: id = -1

        contains

        procedure :: replicate => replicateBase

        final :: finalizeBase
    end type

    type (base(4)), save :: b1_m ! tcx: (4)

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    function replicateBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)

        type(base(4)), pointer :: replicateBase ! tcx: (4)

        allocate (replicateBase)

        replicateBase%id = b%id
    end function

    type (base(4)) function produceBasePtr (id) ! tcx: (4)
        pointer produceBasePtr
        integer*4, intent(in) :: id

        allocate (produceBasePtr)

        produceBasePtr%id = id
    end function
end module

program ffinal514a22k
use m
    class (base(4)), pointer :: b_ptr ! tcx: (4)

    print *, produceBasePtr (10)

    print *, replicateBase (produceBasePtr(2))

    b1_m = produceBasePtr (5)

    if (b1_m%id /= 5) error stop 101_4

    b_ptr => b1_m%replicate()

    if (b_ptr%id /= 5) error stop 2_4

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
