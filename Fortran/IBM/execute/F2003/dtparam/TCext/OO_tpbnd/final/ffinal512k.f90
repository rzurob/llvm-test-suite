! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal512k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal512 by Jim Xia)
!*  DATE                       : 2007-10-12 (original: 04/05/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf90)
!*
!*  DESCRIPTION                : final sub (optinal INTENT(OUT) dummy-arg)
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
        integer(kbase_1) :: data

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'

    end subroutine
end module

program ffinal512k
use m
    interface
        subroutine abc (b)
        use m
            type (base(4)), optional, intent(out) :: b ! tcx: (4)
        end subroutine
    end interface

    type (base(4)) :: b1 ! tcx: (4)

    call abc (b1)

    call abc

    print *, 'end of program'
end

subroutine abc (b)
use m
    type (base(4)), optional, intent(out) :: b ! tcx: (4)

    if (present (b)) print *, 'you should see finalizeBase by now'
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
