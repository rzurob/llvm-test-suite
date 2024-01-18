! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal012a1k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal012a1 by Jim Xia)
!*  DATE                       : 2007-10-31 (original: 05/19/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (finalization of allocated
!*                              allocatables for procedures; not in main prog)
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
        integer(kbase_1) :: x

        contains

        final :: finalizeBase
        final :: finalizeBaseArray
    end type

    type p (kp_1) ! kp_1=4
       integer, kind :: kp_1
        type (base(kp_1)), allocatable :: b(:) ! tcx: (kp_1)
    end type

    contains
    subroutine finalizeBase (b1)
        type (base(4)), intent(inout) :: b1 ! tcx: (4)
        print *, 'in finalizeBase'
    end subroutine

    subroutine finalizeBaseArray (b1)
        type (base(4)), intent(in) :: b1(:) ! tcx: (4)
        print *, 'in finalizeBaseArray'
    end subroutine

end module


program ffinal012a1k
use m
    class (base(4)), allocatable :: b1, b2(:) ! tcx: (4)

    allocate (b1, b2(3))

    print *, 'calling test1'

    call test1

    print *, 'end'
end

subroutine test1
use m
    type (p(4)) :: p1 ! tcx: (4)

    allocate (p1%b(2))
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: p - added parameters (kp_1) to invoke with (4) / declare with (4) - 1 changes
