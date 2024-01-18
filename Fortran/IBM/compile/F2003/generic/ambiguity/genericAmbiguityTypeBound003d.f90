!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : two argument with pass-arg to be first arg specified (for generic-name, operator, and assignment tb)
!*                                  - both args polymorphic
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
         procedure, pass :: pj => printj
         generic :: print => printi, pj
   end type

   contains

      subroutine printi(a, b)
         class(base), intent(in) :: a, b

         print *, a%i, b%i

      end subroutine

      subroutine printj(a, b)
         class(base), intent(in) :: a, b

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
         class(base1), intent(in) :: a, b

         addi%i = a%i + b%i

      end function

      type(base1) function addj(a, b)
         class(base1), intent(in) :: a, b

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
         class(base2), intent(in)  :: b

         a%i = b%i

      end subroutine

      subroutine assgn2(a, b)
         class(base2), intent(out) :: a
         class(base2), intent(in)  :: b

         a%j = b%j

      end subroutine

end module

program genericAmbiguityTypeBound003d
end program
