! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal522ak
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal522a by Jim Xia)
!*  DATE                       : 2007-10-12 (original: 04/14/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (pointer function results are not
!*                               finalized if appears as un-named variables)
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

        procedure :: replicate => replicateBase

        final :: finalizeBase
    end type

    contains

    function replicateBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)
        class (base(4)), pointer :: replicateBase ! tcx: (4)

        type (base(4)), pointer :: temp ! tcx: (4)

        allocate (temp)
        temp%id = b%id

        replicateBase => temp
    end function

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal522ak
use m
    type (base(4)), save :: b1, b2 ! tcx: (4)

    b1%id = 100

    b2 = b1%replicate()

    print *, b2%id
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
