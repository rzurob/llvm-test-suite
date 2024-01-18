! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-11 (original: 04/16/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (other bindings can be called in the
!*                               final sub; also uses IMPLICIT)
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
        integer(kbase_1) :: id = 0

        contains

        final :: finalizeBase
        procedure :: print => printBase
    end type

    contains

    subroutine finalizeBase (b)
        implicit type (base(4)) (b) ! tcx: (4)

        intent(in) :: b

        call b%print
        print *, 'end finalizeBase'
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)

        print *, b%id
    end subroutine
end module

program ffinal524k
    call abc
end

subroutine abc
use m
    implicit type(base(4)) (b) ! tcx: (4)

    target b1

    b1%id = 10
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
