! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Extends keyword, ensure parent component accessibility
!*                                        parent component has private accessibility
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

   type, abstract :: super
      integer :: i
   end type

   type,  abstract, private, extends(super) :: base
      integer :: j
   contains
      procedure, nopass :: print => printbase
   end type

   type, extends(base) :: child
      integer :: r
   end type

   class(child), allocatable :: c1

contains
   subroutine printbase()
      print *, "base"
   end subroutine
end module


program extends004
   use m

   call c1%print()
   allocate(c1, source = child(1,2,3) )

   print *, c1%super%i
   print *, c1%i, c1%j, c1%r

   print *, c1%base%i
   print *, c1%base%j

end program
