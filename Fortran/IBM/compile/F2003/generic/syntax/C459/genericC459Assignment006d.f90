!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( = )
!*
!*  DESCRIPTION                : C459: base type is private, and child type is public
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

   type :: base
      character(3) :: c
      contains
         generic, public :: assignment(=) => typetotype
         procedure, pass, private :: typetotype => btob
   end type

   type, extends(base) :: child
      integer :: i
      contains
         procedure, pass ::  ctoi
         generic, private :: assignment(=) => ctoi
   end type

   contains

   subroutine btob ( a , b )
      class(base) :: a, b
      intent(out) :: a
      intent(in) :: b

      a%c = b%c

   end subroutine

   subroutine ctoi ( a , b )
      class(child) :: a
      integer :: b
      intent(out) :: a
      intent(in) :: b

      a%i = b

   end subroutine

end module

program genericC459Assignment006d
end program
