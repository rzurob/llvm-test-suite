!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal019ak.f
!*  TEST CASE NAME             : type-bound procedure ffinal019ak
!*
!*  DATE                       : 2007-11-26 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing final subroutines:defect 284803
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type :: base (kbase_1) ! kbase_1=4
      integer, kind :: kbase_1
      integer(kbase_1) :: x
   end type

end module

module m1
   use m

   type,extends(base) :: child
   contains
      final :: finalChild
   end type

   type(child(8)), allocatable :: dt0  ! tcx: (8)

contains
   subroutine finalChild(arg1)
      type(child(8)),intent(inout) :: arg1 ! tcx: (8)
      print *, "finalizeChild"
   end subroutine

end module

   use m1

   call example

 end

   subroutine example()

      use m1

      type(child(8)) :: dt1 ! tcx: (8)

     allocate(dt0)
     deallocate(dt0)

   end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 0 changes
! type: child - added parameters () to invoke with (8) / declare with (8) - 3 changes
