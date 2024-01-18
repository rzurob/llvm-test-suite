! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal004a1k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal004a1 by Jim Xia)
!*  DATE                       : 2007-10-31 (original: 02/07/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final subroutine (explict-shape array as the
!                               dummy-arg for the final sub)
!*
!*  KEYWORD(S)                 :
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
        integer(kbase_1) :: flag

        contains

        final :: finalizeBase, finalizeBaseArray1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base(4)), intent(inout) :: b(2)   ! this is an explicit-shape ! tcx: (4)

        print *, 'finalizer for rank 1 array of base type'
        print *, b%flag
    end subroutine

end module

program ffinal004a1k
use m

    type (base(4)), pointer :: b1_ptr ! tcx: (4)
    type (base(4)), allocatable :: b1_allo ! tcx: (4)

    type (base(4)), pointer :: b2_ptr(:) ! tcx: (4)
    type (base(4)), allocatable :: b2_allo (:) ! tcx: (4)

    allocate (b1_ptr, b1_allo)

    allocate (b2_ptr(3), b2_allo(4))

    b2_ptr%flag = (/1,2,3/)

    deallocate (b1_ptr, b2_ptr)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
