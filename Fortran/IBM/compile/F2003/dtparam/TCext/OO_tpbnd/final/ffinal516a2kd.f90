! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal516a2kd
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal516a2 by Jim Xia)
!*  DATE                       : 2007-11-11 (original: 06/15/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : was "final sub (finalization of function results in specification expression)"
!*                               now this is a diagnostic, verifying that the private length parameter is
!*                               correctly handled when referenced in a structure constructor, despite
!*                               interference from an identically-named interface.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), private :: length

        contains

        procedure :: computeLength
        final :: finalizeBase
    end type

    interface base
            module procedure createBaseObj
    end interface

    contains

    pure type(base(4)) function createBaseObj (len) ! tcx: (4)
        integer,intent(in) :: len

        createBaseObj%length = len
    end function

    pure integer function computeLength(b, s)
        class(base(4)), intent(in) :: b ! tcx: (4)
        character(*), intent(in) :: s

        computeLength = len(s)
    end function

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal516a2kd
use m
    print *, 'start main'

    call test1

    print *, 'calling test2'

    call test2 (base(4)(100)) ! tcx: (4) - invoke structure constructor

    print *, 'end'

    contains

    subroutine test2 (b1)
        type(base(4)), intent(in) :: b1 ! tcx: (4)
        character(b1%computeLength('again test')) s2

        print *, 'starting test2'

        if (len(s2) /= len('again test')) error stop 2_4
        print *, 'leaving test2'
    end subroutine
end

subroutine test1
use m
    character(computeLength(base(4)(10), "test1")) s1 ! tcx: (4) - invoke structure constructor

    print *, 'start test1'

    if (len(s1) /= len('test1')) error stop 101_4

    print *, 'end of test1'
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 7 changes
