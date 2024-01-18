!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : two argument with pass-arg to be first arg specified (for generic-name, operator, and assignment tb)
!*                                  - one arg polymorphic, the other non-polymorphic of the same type
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

module genericName

   type base
      integer :: i, j
      contains
         procedure, pass :: printi
         procedure, pass :: printj
         generic :: print => printi, printj
   end type

   contains

      subroutine printi(a, b)
         class(base), intent(in) :: a
         type(base), intent(in) :: b

         print *, a%i, b%i

      end subroutine

      subroutine printj(a, b)
         class(base), intent(in) :: a
         type(base), intent(in)  :: b

         print *, a%j, b%j

      end subroutine

end module

module binoperator

   type base1
      integer :: i, j
      contains
         procedure, pass :: addi
         procedure, pass :: addj
         generic :: operator(+) => addi, addj
   end type

   contains

      type(base1) function addi(a, b)
         class(base1), intent(in) :: a
         type(base1), intent(in)  :: b

         addi%i = a%i + b%i

      end function

      type(base1) function addj(a, b)
         class(base1), intent(in) :: a
         type(base1), intent(in)  :: b

         addj%i = a%j + b%j

      end function

end module

module assignment

   type base2
     integer :: i, j
      contains
         procedure, pass :: assgn1
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn1, assgn2
   end type

   contains

      subroutine assgn1(a, b)
         class(base2), intent(out) :: a
         type(base2), intent(in)  :: b

         a%i = b%i

      end subroutine

      subroutine assgn2(a, b)
         class(base2), intent(out) :: a
         type(base2), intent(in)  :: b

         a%j = b%j

      end subroutine

end module

program genericAmbiguityTypeBound004d
end program
