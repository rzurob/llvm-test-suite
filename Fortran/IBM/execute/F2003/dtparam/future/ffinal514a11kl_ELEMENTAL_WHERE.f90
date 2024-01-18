! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-02 (original: 02/11/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of the temporary
!                               created inside the where statement)
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

program ffinal514a11kl
use m1
    type (base(4)), save :: b1(10) ! tcx: (4)
    type (child(4,20)), save :: c1(2:6) ! tcx: (4,20)

    b1%id = (/(i, i=1,10)/)

    print *, 'entering where statement'

    where (b1 == makeData (5))  b1%id = -b1%id

    if (any(b1%id /= (/1,2,3,4,-5,6,7,8,9,10/))) error stop 101_4

    print *, 'done with b1'

    c1%id = (/(i*10, i=2,6)/)

    c1%name = 'c1_static'

    where (c1 == makeData (30, 'c1_static')) c1%id = -30

    if (any (c1%id /= (/20, -30, 40, 50, 60/))) error stop 2_4

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
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 6 changes
