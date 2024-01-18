! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal524ak
!*
!*  DATE                       : 2007-11-11 (original: 04/16/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (other bindings can be invoked in
!*                               final sub; modify in this case)
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
        integer(kbase_1) :: id = 1

        contains

        final :: finalizeBase
        procedure :: print => printBase
        procedure :: add1 => addOne2id
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)) b ! tcx: (4)

        intent(inout) :: b

        call b%add1
        call b%print
        print *, 'end finalizeBase'
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)

        print *, b%id
    end subroutine

    subroutine addOne2id (b)
        class (base(4)), intent(inout) :: b ! tcx: (4)

        b%id = b%id + 1
    end subroutine
end module

program ffinal524ak
    call abc

    call abc
end

subroutine abc
use m
    implicit type(base(4)) (b) ! tcx: (4)

    target b1
    logical, save :: firstTime = .true.

    if (firstTime) then
        b1%id = 10
        firstTime = .false.
    end if
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
