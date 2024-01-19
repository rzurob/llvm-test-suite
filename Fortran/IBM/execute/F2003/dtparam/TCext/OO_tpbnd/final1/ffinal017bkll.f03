!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-26 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing final subroutines: final
!*                               subroutines are not inherited
!*                               through type extension.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: x
        contains
        final :: finalizeBase
    end type

    type, extends(base) :: parent (lparent) ! lparent=0
       integer, len :: lparent
    end type

    type, extends(parent) :: child (lchild) ! lchild=0
       integer, len :: lchild
    contains
       final :: finalizeChild
    end type

    contains
    subroutine finalizeBase (b1)
        type (base(4)), intent(inout) :: b1 ! tcx: (4)
        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (b1)
        type (child(4,*,*)), intent(inout) :: b1 ! tcx: (4,*,*)
        print *, 'finalizeChild'
    end subroutine

end module
    call sub()

end

subroutine sub()
    use m
    type(child(4,:,:)), pointer :: t_c1 ! tcx: (4,:,:)
    allocate(child(4,0,0)::t_c1) ! tcx: child(4,0,0)
    deallocate(t_c1)
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
! type: parent - added parameters (lparent) to invoke with (4,0) / declare with (4,*) - 0 changes
! type: child - added parameters (lchild) to invoke with (4,0,0) / declare with (4,*,*) - 2 changes
