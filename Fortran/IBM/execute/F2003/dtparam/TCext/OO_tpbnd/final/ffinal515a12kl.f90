! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal515a12kl
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal515a12 by Jim Xia)
!*  DATE                       : 2007-11-07 (original: 04/27/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (temps created by structure
!*                               constructor in FORALL statement)
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
        integer(kbase_1) :: id = -1

        contains

        final :: finalizeBase
        procedure :: isDefault => isBaseWithDefault
    end type

    type, extends (base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name = 'no-name'

        contains

        final :: finalizeChild
        procedure :: isDefault => isChildWithDefault
    end type

    interface operator(==)
        pure logical function baseEqual (b1, b2)
        import base
            class (base(4)), intent(in) :: b1 ! tcx: (4)
            type (base(4)), intent(in) :: b2 ! tcx: (4)
        end function

        pure logical function childEqual (c1, c2)
        import child
            class (child(4,*)), intent(in) :: c1 ! tcx: (4,*)
            type (child(4,*)), intent(in) :: c2 ! tcx: (4,*)
        end function
    end interface

    contains

    pure subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        b%id = -1
    end subroutine

    pure subroutine finalizeChild (c)
        type (child(4,*)), intent(inout) :: c ! tcx: (4,*)

        c%name = 'no-name'
    end subroutine

    pure logical function isBaseWithDefault (b)
        class (base(4)), intent(in) :: b ! tcx: (4)

        isBaseWithDefault = (b%id == -1)
    end function

    pure logical function isChildWithDefault (b)
        class (child(4,*)), intent(in) :: b ! tcx: (4,*)

        isChildWithDefault = (b%base%isDefault() .and. (b%name == 'no-name'))
    end function
end module

pure logical function baseEqual (b1, b2)
use m, only: base
    class (base(4)), intent(in) :: b1 ! tcx: (4)
    type (base(4)), intent(in) :: b2 ! tcx: (4)

    baseEqual = (b1%id == b2%id)
end function

pure logical function childEqual (c1, c2)
use m, only: child, base, operator(==)
    class (child(4,*)), intent(in) :: c1 ! tcx: (4,*)
    type (child(4,*)), intent(in) :: c2 ! tcx: (4,*)

    childEqual = ((c1%base == c2%base) .and. (c1%name == c2%name))
end function

program ffinal515a12kl
use m
    type (child(4,20)) :: c1 (10) ! tcx: (4,20)

    print *, 'begin'

    forall (i=1:10, c1(i)%isDefault())  c1(i)%id = i

    print *, 'second forall'

    !! Note that there may be only one temp in this forall statement
    forall (i=1:3, c1(i) == child(4,20) (1))  c1(i)%name = 'test' ! tcx: (4,20)

    if (any (c1%id /= (/1,2,3,4,5,6,7,8,9,10/))) error stop 101_4

    if (c1(1)%name /= 'test') error stop 2_4

    if (any (c1(2:)%name /= 'no-name')) error stop 3_4
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 8 changes
