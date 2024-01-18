!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines:defect 284803
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type :: base
      integer :: x
   end type

end module

module m1
   use m

   type,extends(base) :: child
   contains
      final :: finalChild
   end type

   type(child), allocatable :: dt0

contains
   subroutine finalChild(arg1)
      type(child),intent(inout) :: arg1
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

