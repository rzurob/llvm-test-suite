! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Extends keyword, ensure parent component accessibility
!*                                        parent component and parent's component have private accessibility
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
   type, abstract, private :: base
      integer, private :: i = 5
   contains
      procedure, pass :: print => printbase
   end type

   type, extends(base) :: child
      integer :: r
   end type

   class(child), allocatable :: c1

contains
   subroutine printbase(a)
      class(base), intent(in) :: a
      print *, a%i
   end subroutine
end module


program extends006
   use m

   allocate(c1, source = child(r=7))
   print *,c1%r
   call c1%print()

end program