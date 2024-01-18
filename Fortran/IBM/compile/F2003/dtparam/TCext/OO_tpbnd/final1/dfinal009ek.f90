!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal009ek.f
!*  TEST CASE NAME             : type-bound procedure dfinal009ek
!*
!*  DATE                       : 2007-11-13 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing the dummy argument of final
!*                               subroutine : may not have the SAVE
!*                               attribute.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type ::  base (kbase_1) ! kbase_1=4
      integer, kind :: kbase_1
   integer(kbase_1) :: x
   contains
      final :: finalBase
   end type

contains
   subroutine finalBase(arg1)
      type(base(4)),intent(inout) :: arg1 ! tcx: (4)
!* expect error message 1514-018 here
      save :: arg1
      print *, "finalizeBase"
   end subroutine

end module

module m1
   use m

   type,extends(base) :: child
      integer(kbase_1) :: x

   contains
      final :: finalChild
   end type

   type(child(4)), allocatable :: dt0  ! tcx: (4)

contains
   subroutine finalChild(arg1)
      type(child(4)),intent(inout) :: arg1 ! tcx: (4)
!* expect error message 1514-018 here
      save :: arg1
      print *, "finalizeChild"
   end subroutine

end module

   use m1

   call example

 end

   subroutine example()
      use m1
      type(base(4)) :: dt1 ! tcx: (4)

      allocate(dt0)
      deallocate(dt0)

   end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 2 changes
