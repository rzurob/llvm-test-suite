!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal009clk.f
!*  TEST CASE NAME             : type-bound procedure dfinal009clk
!*
!*  DATE                       : 2007-11-13 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing the dummy argument of final
!*                               subroutines:
!*                               with value attribute
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type :: base (lb) ! lb=0
      integer, len :: lb
   end type

end module

module m1
   use m

   type,extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
   integer(kchild_1) :: x
   contains
      final :: finalChild
   end type

   type(child(:,4)), allocatable :: dt0  ! tcx: (:,4)

contains
   subroutine finalChild(arg1)
      type(child(0,4))  :: arg1 ! tcx: (0,4)
      value :: arg1
      print *, "finalizeChild"
   end subroutine

end module

   use m1

   call example

 end

   subroutine example()

      use m1

      type(child(0,4)) :: dt1 ! tcx: (0,4)

      allocate(child(0,4)::dt0) ! tcx: child(0,4)
      deallocate(dt0)

   end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (0) / declare with (*) - 0 changes
! type: child - added parameters (kchild_1) to invoke with (0,4) / declare with (*,4) - 3 changes
