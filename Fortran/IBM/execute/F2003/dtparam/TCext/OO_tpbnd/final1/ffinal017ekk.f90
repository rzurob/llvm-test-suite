!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal017ekk.f
!*  TEST CASE NAME             : type-bound procedure ffinal017ekk
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

    type, extends(base) :: child (kchild_1) ! kchild_1=4
       integer, kind :: kchild_1
       type(base(kchild_1)), pointer :: dt_b ! tcx: (kchild_1)
    contains
       final :: finalizeChild
    end type

    contains
    subroutine finalizeBase (b1)
        type (base(4)), intent(inout) :: b1 ! tcx: (4)
        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (b1)
        type (child(4,4)), intent(inout) :: b1 ! tcx: (4,4)
        print *, 'finalizeChild'
    end subroutine

end module

   use m
   call sub()

end

subroutine sub()
    use m
    type(child(4,4)) :: t_c1 ! tcx: (4,4)
  ! allocate(t_c1)
    allocate(t_c1%dt_b)
  ! deallocate(t_c1)
    deallocate(t_c1%dt_b)
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (kchild_1) to invoke with (4,4) / declare with (4,4) - 2 changes
