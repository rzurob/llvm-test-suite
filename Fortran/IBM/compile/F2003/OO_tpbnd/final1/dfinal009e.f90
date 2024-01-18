!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp dfinal009e.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal009e.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing the dummy argument of final
!*                               subroutine : may not have the SAVE
!*                               attribute.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type ::  base
   integer :: x
   contains
      final :: finalBase
   end type

contains
   subroutine finalBase(arg1)
      type(base),intent(inout) :: arg1
!* expect error message 1514-018 here
      save :: arg1
      print *, "finalizeBase"
   end subroutine

end module

module m1
   use m

   type,extends(base) :: child
   integer :: x

   contains
      final :: finalChild
   end type

   type(child), allocatable :: dt0

contains
   subroutine finalChild(arg1)
      type(child),intent(inout) :: arg1
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
      type(base) :: dt1

      allocate(dt0)
      deallocate(dt0)

   end subroutine

