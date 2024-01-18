!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal009akk.f
!*  TEST CASE NAME             : type-bound procedure dfinal009akk
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
!*                               the ALLOCATABLE attribute
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type :: base (kb) ! kb=4
      integer, kind :: kb
   end type

end module

module m1
   use m

   type,extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
   contains
      final :: finalChild
   end type

   type(child(4,4)), allocatable :: dt0  ! tcx: (4,4)

contains
   subroutine finalChild(arg1)
      type(child(4,4)),intent(inout), allocatable :: arg1 ! tcx: (4,4)
      print *, "finalizeChild"
   end subroutine

end module

   use m1

   call example

 end

   subroutine example()

      use m1

      type(child(4,4)) :: dt1 ! tcx: (4,4)

      allocate(dt0)
      deallocate(dt0)

   end subroutine



! Extensions to introduce derived type parameters:
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 2 changes
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 0 changes
! type: child - added parameters (kc) to invoke with (4,4) / declare with (4,4) - 3 changes
