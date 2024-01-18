! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal514ak
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal514a by Jim Xia)
!*  DATE                       : 2007-10-12 (original: 04/12/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (function return result is finalized
!*                               after the use in defined assignment)
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
        integer(kbase_1), pointer :: id => null()

        contains

        procedure :: replicate => produceBase

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'

        if (associated (b%id)) then
            print *, 'deallocating data'
            deallocate (b%id)
        end if
    end subroutine

    type (base(4)) function produceBase (b) ! tcx: (4)
        class (base(4)), intent(in) :: b ! tcx: (4)

        allocate (produceBase%id)

        produceBase%id = b%id
    end function
end module

program ffinal514ak
use m
    interface assignment (=)
        subroutine base2Base (b1, b2)
        use m
            type (base(4)), intent(out) :: b1 ! tcx: (4)
            type (base(4)), intent(in) :: b2 ! tcx: (4)

        end subroutine
    end interface

    type (base(4)), save :: b1, b2 ! tcx: (4)

    allocate (b1%id)

    b1%id = 100

    b2 = b1%replicate()

    print *, 'end'
end

subroutine base2Base (b1, b2)
use m
    type (base(4)), intent(out) :: b1 ! tcx: (4)
    type (base(4)), intent(in) :: b2 ! tcx: (4)

    if (associated (b2%id)) then
        allocate (b1%id)

        b1%id = b2%id
    end if
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 8 changes
