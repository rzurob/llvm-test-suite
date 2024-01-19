!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguous but between interface and deferred type bound
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module binoperator

   type, abstract :: base1
      integer :: i
      contains
         procedure(addb), pass, deferred :: adda
         generic :: operator(*) => adda
   end type

   interface operator(*)
      class(base1) function addb(a, b)
         import base1
         class(base1), intent(in) :: a
         class(base1), intent(in)  :: b
         allocatable :: addb
      end function
   end interface

   type, extends(base1) :: child1
      contains
         procedure, pass :: adda
   end type

   contains

      class(base1) function adda(a, b)
         class(child1), intent(in) :: a
         class(base1), intent(in)  :: b
         allocatable :: adda

         allocate ( child1 :: adda )
      end function

end module

class(base1) function addb(a, b)
   use binoperator, only: base1, child1

   class(base1), intent(in) :: a
   class(base1), intent(in)  :: b
   allocatable :: addb

   allocate ( child1 :: addb )

end function

program genericAmbiguityInterface010d
end program

