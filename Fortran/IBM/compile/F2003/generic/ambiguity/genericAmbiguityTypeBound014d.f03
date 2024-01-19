!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : two argument with pass-arg to be first arg specified (for generic-name, operator, and assignment tb)
!*                                  - one arg being unlimited polymorphic, the ambiguous tb has class(*) arg
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

   type b1
      integer :: i
      contains
         procedure :: printa
         procedure :: printb

         generic :: print => printa, printb

   end type

   contains

      subroutine printa(a, b)
         class(b1), intent(in) :: a
         class(*),  intent(in) :: b

      end subroutine

      subroutine printb(a, b)
         class(b1), intent(in)  :: a
         class(*), intent(in) :: b

      end subroutine

end module

module binoperator

   type b11
      integer :: i
      contains
         procedure, pass :: adda
         procedure, pass :: addb
         generic :: operator(+) => adda, addb
   end type

   contains

      type(b11) function adda(a, b)
         class(b11), intent(in) :: a
         class(*), intent(in) :: b

         adda = b11(10)

      end function

      type(b11) function addb(a, b)
         class(b11), intent(in) :: a
         class(*), intent(in) :: b

         addb = b11(20)

      end function

end module

module assignment

   type b12
     integer :: i, j
      contains
         procedure, pass :: assgn1
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn1, assgn2
   end type

   contains

      subroutine assgn1(a, b)
         class(b12), intent(out) :: a
         class(*), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(b12), intent(out) :: a
         class(*), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound014d
end program
