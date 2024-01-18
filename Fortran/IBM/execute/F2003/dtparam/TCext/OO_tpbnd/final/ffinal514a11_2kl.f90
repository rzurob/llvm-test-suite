! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal514a11_2kl
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal514a11_2 by Jim Xia)
!*  DATE                       : 2007-11-02 (original: 04/19/2004)
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
!*                               function calls during where statement; a
!*                               reduced test case from ffinal514a11.f)
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
    end type

    interface makeData
        function makeBase (i)
        import base
            type(base(4)) :: makeBase ! tcx: (4)
            integer*4, intent(in) :: i
        end function
    end interface

    interface operator (==)
        elemental logical function baseEqual (b1, b2)
        import base
            type (base(4)), intent(in) :: b1, b2 ! tcx: (4)
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine
end module

module m1
use m
    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name = 'no-name'

        contains

        final :: finalizeChild
    end type

    interface makeData
        function makeChildObj (i, c)
        import child
            type (child(4,20)) makeChildObj ! tcx: (4,20)
            integer*4, intent(in) :: i
            character(*), intent(in) :: c
        end function
    end interface

    interface operator (.eq.)
        elemental logical function childEqual (c1, c2)
        import child
            type (child(4,*)), intent(in) :: c1, c2 ! tcx: (4,*)
        end function
    end interface

    contains

    subroutine finalizeChild (c)
        type (child(4,*)), intent(in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal514a11_2kl
use m1
    type (child(4,20)), save :: c1(2:6) ! tcx: (4,20)

    c1%id = (/(i*10, i=2,6)/)

    c1%name = 'c1_static'

    print *, (c1 == makeData (30, 'c1_static'))

    print *, 'end'
end

function makeBase (i)
use m, only: base
    type(base(4)) :: makeBase ! tcx: (4)
    intent(in) i

    makeBase%id = i
end function

function makeChildObj (i, c)
use m1, only: child
    type (child(4,20)) makeChildObj ! tcx: (4,20)
    integer*4, intent(in) :: i
    character(*), intent(in) :: c

    makeChildObj%id = i
    makeChildObj%name = c
end function

elemental logical function baseEqual (b1, b2)
use m, only:base
    type (base(4)), intent(in) :: b1, b2 ! tcx: (4)

    baseEqual = (b1%id == b2%id)
end function

elemental logical function childEqual (c1, c2)
use m1, only: base, child, operator(==)
    type (child(4,*)), intent(in) :: c1, c2 ! tcx: (4,*)

    childEqual = ((c1%base == c2%base) .and. (c1%name == c2%name))
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 6 changes
