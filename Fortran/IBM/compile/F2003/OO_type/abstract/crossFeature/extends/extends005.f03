! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Extends keyword, ensure parent component accessibility
!*                                        parent's data component has private accessibility
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type, abstract :: base
      integer, private :: i = 5
   contains
      procedure, pass :: print => printbase
   end type

   type, extends(base) :: child
      real :: r
   end type


contains

   subroutine printbase(a)
      class(base), intent(in) :: a
      print *, a%i
   end subroutine

end module


program extends005
   use m

   type(child) :: c1
   class(child), allocatable :: c2

   call c1%print()
   print *, c1%i
   allocate(c2, source = child(5,5.5) )


end program