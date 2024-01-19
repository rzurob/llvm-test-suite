!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : two argument with pass-arg to be first/second arg specified (for generic-name, operator, and assignment tb)
!*                                  - one different independent types, ambiguous type bound, but type do not collide
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

module m

   type b1
      integer :: i
      contains
         procedure, pass(a) :: printa
         generic :: print => printa
   end type

   type b2
      integer :: i
      contains
         procedure, pass(b) :: printb
         generic :: print => printb
   end type

   contains

      subroutine printa(a, b)
         class(b1), intent(in) :: a
         type(b2),  intent(in) :: b

         print *, 'printa: ', a%i, b%i

      end subroutine

      subroutine printb(a, b)
         type(b1), intent(in)  :: a
         class(b2), intent(in) :: b

         print *, 'printb: ', a%i, b%i

      end subroutine

end module

program genericAmbiguityTypeBound012
   use m

   type(b1) :: b11 = b1(10)
   type(b2) :: b12 = b2(20)

   call b11%print(b12)
   call b12%print(b11)

end program
