!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-12 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing final subroutines:
!*                               The dummy argument of the final
!*                               subroutine finalchild must not have
!*                               the OPTIONAL attribute
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type :: base
   end type

end module

module m1
   use m

   type,extends(base) :: child (lchild) ! lchild=0
      integer, len :: lchild
   contains
      final :: finalChild
   end type

   type(child(:)), allocatable :: dt0  ! tcx: (:)

contains
   subroutine finalChild(arg1)
      type(child(*)),intent(inout), optional :: arg1 ! tcx: (*)
      print *, "finalizeChild"
   end subroutine

end module

   use m1

   call example

 end

   subroutine example()

      use m1

      type(child(0)) :: dt1 ! tcx: (0)

      allocate(child(0)::dt0) ! tcx: child(0)
      deallocate(dt0)

   end subroutine



! Extensions to introduce derived type parameters:
! type: child - added parameters (lchild) to invoke with (0) / declare with (*) - 3 changes
