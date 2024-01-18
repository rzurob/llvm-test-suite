! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal514a6_1k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal514a6_1 by Jim Xia)
!*  DATE                       : 2007-11-07 (original: 04/15/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (finalizatiion of the temporaries
!*                               created by function calls in if-stmt)
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
        procedure, nopass :: makeObj => produceBase
    end type

    interface
        function produceBase (i)
        import base
            type (base(4)) :: produceBase ! tcx: (4)
            integer*4, intent(in) :: i
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module


program ffinal514a6_1k
use m
    interface operator (==)
        pure logical function baseEqual (b1, b2)
        use m
            type (base(4)), intent(in) :: b1, b2 ! tcx: (4)
        end function
    end interface

    type(base(4)), save :: b1 ! tcx: (4)

    b1%id = 10

    if (b1 == b1%makeObj(11))   error stop 1_4

    print *, 'end'
end


logical function baseEqual (b1, b2)
use m
    type (base(4)), intent(in) :: b1, b2 ! tcx: (4)

    baseEqual = (b1%id == b2%id)
end function


type (base(4)) function produceBase (i) ! tcx: (4)
use m
    integer*4, intent(in) :: i

    produceBase%id = i
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
