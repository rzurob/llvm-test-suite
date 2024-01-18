! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal514a8kl
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal514a8 by Jim Xia)
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
!*  DESCRIPTION                : final sub (finalization of temps created by
!*                               function result in PRINT statement)
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

    private finalizeBase

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent (in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        final :: finalizeChild
    end type

    interface produceObj
        function produceBase (i)
        use m
            type (base(4)) produceBase ! tcx: (4)
            integer*4, intent(in) :: i
        end function

        function produceChildObj (i, c)
        import child
            type (child(4,20)) produceChildObj ! tcx: (4,20)
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function
    end interface

    contains

    subroutine finalizeChild (c)
        type (child(4,*)), intent (in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal514a8kl
use m1

    print *, produceObj (10)
    print *, produceObj (10, 'test1')
    print *, 'end'
end

function produceBase (i)
use m
    type (base(4)) produceBase ! tcx: (4)
    integer*4, intent(in) :: i

    produceBase%id = i
end function

function produceChildObj (i, c)
use m1, only : child
    type (child(4,20)) produceChildObj ! tcx: (4,20)
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    produceChildObj%id = i
    produceChildObj%name = c
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 3 changes
