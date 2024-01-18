!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg005.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*                                         v)polymorphic abstract type pointer/allocatable being dummy argument
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   end type

   type, extends(base) :: child(k2)
      integer, kind :: k2
      real(k2) :: rid
   end type

contains

   subroutine associateBasePointer(a,atarget)
      class(base(4)), pointer, intent(inout) :: a
      class(base(4)), intent(in), target :: atarget
      a => atarget
   end subroutine

end module

program dummyArg005
   use m

   class(base(4)), pointer :: b1
   class(base(4)), allocatable, target :: b2
   allocate(b2, source = child(4,4)(3,4.5))

   call associateBasePointer(b1,b2)

   if (.not. associated(b1,b2) ) error stop 1_4

end program
