! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal007al2
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal007a by Jim Xia)
!*  DATE                       : 2007-10-31 (original: 02/09/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (parent components finalizations in
!                               step 3; use array entities)
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
    type base (lBase) ! lBase=15
       integer, len :: lBase
        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(lBase) :: name
    end type

    contains

    subroutine finalizeBase (b)
        type (base(*)), intent(in) :: b ! tcx: (*)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal007al2
use m
    type (child(:)), pointer :: c1(:,:) ! tcx: (:)

    allocate (child(15)::c1(2,2)) ! tcx: child(15)

    deallocate (c1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (lBase) to invoke with (15) / declare with (*) - 1 changes
