! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal513k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal513 by Jim Xia)
!*  DATE                       : 2007-10-12 (original: 04/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf90)
!*
!*  DESCRIPTION                : final sub (very basic test for intent(out)
!*                               finalization)
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
        integer(kbase_1), pointer :: data

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        if (associated(b%data)) deallocate (b%data)
    end subroutine
end module

use m
    type (base(4)):: b1 ! tcx: (4)

    allocate (b1%data)

    call abc (b1)

    if (associated (b1%data)) error stop 101_4

    allocate (b1%data)

    call cba (b1)

    if (associated (b1%data)) error stop 2_4

    contains

    subroutine abc (b)
        type (base(4)), intent(out) :: b ! tcx: (4)
    end subroutine

    subroutine cba (b)
        class (base(4)), intent(out) :: b ! tcx: (4)
    end subroutine
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
