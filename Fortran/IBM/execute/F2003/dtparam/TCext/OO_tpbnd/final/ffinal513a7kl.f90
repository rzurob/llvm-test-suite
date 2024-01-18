! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal513a7kl
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal513a7 by Jim Xia)
!*  DATE                       : 2007-10-31 (original: 06/24/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (autodeallocation of allocatable
!                               dummy-arg with INTENT(OUT))
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

        final :: finalizeBase, finalizeBaseRank1
        procedure :: isDefault => isBaseDefault
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name = 'no-name'

        contains

        final :: finalizeChild, finalizeChildRank1
        procedure :: isDefault => isChildDefault
    end type

    contains

    logical function isBaseDefault (b)
        class (base(4)), intent (in) :: b ! tcx: (4)

        isBaseDefault = (b%id == -1)
    end function

    logical function isChildDefault (b)
        class (child(4,*)), intent (in) :: b ! tcx: (4,*)

        isChildDefault = (b%base%isDefault() .and. (b%name == 'no-name'))
    end function

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(4,*)), intent(in) :: c(:) ! tcx: (4,*)

        print *, 'finalizeChildRank1'
    end subroutine

    integer*4 function abc (b)
        class (base(4)), allocatable, intent(out) :: b ! tcx: (4)

        if (allocated (b)) error stop 10_4

        abc = 1
    end function

    integer*4 function cba (b)
        class (base(4)), allocatable, intent(out) :: b(:) ! tcx: (4)

        if (allocated (b)) error stop 11_4

        cba = 2
    end function
end module

program ffinal513a7kl
use m
    class (base(4)), allocatable :: b1, b2(:) ! tcx: (4)

    allocate (b1, source=child(4,20)()) ! tcx: (4,20)

    allocate (child(4,20) :: b2(3)) ! tcx: (4,20)

    if (.not. b1%isDefault()) error stop 101_4

    if (size (b2) /= 3) error stop 2_4

    do i = 1, 3
        if (.not. b2(i)%isDefault())  error stop 3_4
    end do

    i1 =  abc (b1)

    i2 =  cba (b2)

    if ((i1 /= 1) .or. (i2 /= 2)) error stop 4_4

    if (allocated (b1) .or. allocated (b2)) error stop 5_4
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 5 changes
