!**********************************************************************
!*  ===================================================================
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

   type ::  base (lbase) ! lbase=1
      integer, len :: lbase
   contains
      final :: finalBase
   end type

contains
   subroutine finalBase(arg1)
      type(base(*)),intent(inout) :: arg1 ! tcx: (*)
      print *, "finalizeBase"
   end subroutine

end module

module m1
   use m

   type,extends(base) :: child (kchild) ! kchild=16
      integer, kind :: kchild
   contains
      final :: finalChild
   end type

   type(child(:,16)), allocatable :: dt0  ! tcx: (:,16)

contains
   subroutine finalChild(arg1)
      type(child(*,16)),intent(inout) :: arg1 ! tcx: (*,16)
      print *, "finalizeChild"
   end subroutine

end module

   use m1

   call example

 end

   subroutine example()

      use m1

      type(base(1)) :: dt1 ! tcx: (1)

      allocate(child(1,16)::dt0) ! tcx: child(1,16)
      deallocate(dt0)


   end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase) to invoke with (1) / declare with (*) - 2 changes
! type: child - added parameters (kchild) to invoke with (1,16) / declare with (*,16) - 2 changes