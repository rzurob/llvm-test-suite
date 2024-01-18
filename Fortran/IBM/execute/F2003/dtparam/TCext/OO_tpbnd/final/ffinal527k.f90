! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal527k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal527 by Jim Xia)
!*  DATE                       : 2007-11-11 (original: 04/23/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (elemental final sub is used for
!                               arrays if there is no matching rank finalizer)
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

    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        type (base(kdataType_1)), pointer :: data ! tcx: (kdataType_1)

        contains

        final :: finalizeData
    end type

    contains

    pure subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

    end subroutine

    elemental subroutine finalizeData (d)
        type (dataType(4)), intent(inout) :: d ! tcx: (4)

        if (associated (d%data)) deallocate (d%data)
    end subroutine
end module

program ffinal527k
use m
    type (dataType(4)) :: d1(3) ! tcx: (4)

    allocate (d1(1)%data, d1(2)%data, d1(3)%data)

    call abc (d1)

    contains

    subroutine abc (b)
        class (dataType(4)), intent(out) :: b(:) ! tcx: (4)

        do i = 1, size (b)
            if (associated (b(i)%data)) error stop 101_4
        end do
    end subroutine
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 3 changes
