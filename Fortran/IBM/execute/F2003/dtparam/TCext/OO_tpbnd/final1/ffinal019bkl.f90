!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal019bkl.f
!*  TEST CASE NAME             : type-bound procedure ffinal019bkl
!*
!*  DATE                       : 2007-11-26 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing final subroutines: defect 284803
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type ::  base (kbase) ! kbase=16
      integer, kind :: kbase
   contains
      final :: finalBase
   end type

contains
   subroutine finalBase(arg1)
      type(base(16)),intent(inout) :: arg1 ! tcx: (16)
      print *, "finalizeBase"
   end subroutine

end module

module m1
   use m

   type,extends(base) :: child (lchild) ! lchild=1
      integer, len :: lchild
   contains
      final :: finalChild
   end type

   type(child(16,:)), allocatable :: dt0  ! tcx: (16,:)

contains
   subroutine finalChild(arg1)
      type(child(16,*)),intent(inout) :: arg1 ! tcx: (16,*)
      print *, "finalizeChild"
   end subroutine

end module

   use m1

   call example

 end

   subroutine example()

      use m1

      type(base(16)) :: dt1 ! tcx: (16)

      allocate(child(16,1)::dt0) ! tcx: child(16,1)
      deallocate(dt0)


   end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase) to invoke with (16) / declare with (16) - 2 changes
! type: child - added parameters (lchild) to invoke with (16,1) / declare with (16,*) - 2 changes
