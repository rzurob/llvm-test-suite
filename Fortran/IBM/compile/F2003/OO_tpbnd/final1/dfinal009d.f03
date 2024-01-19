!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing the dummy argument of final
!*                               subroutines must not be polymorphic.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type :: base
   end type

end module

module m1
   use m

   type,extends(base) :: child
   contains
!* expect error message 1514-597 here
      final :: finalChild
   end type

   type(child), allocatable :: dt0

contains
   subroutine finalChild(arg1)
      class(child),intent(inout)  :: arg1
      private :: arg1
      print *, "finalizeChild"
   end subroutine

end module

   use m1

   call example

 end

   subroutine example()

      use m1

      type(child) :: dt1

      allocate(dt0)
      deallocate(dt0)

   end subroutine

