!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguous but between interface and type bounds
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

   type base1
      integer :: i
      contains
         procedure, pass :: adda
         generic :: operator(*) => adda
   end type

   type, extends(base1) :: child1
      integer :: j
   end type

   interface operator(*)
      procedure addb
   end interface

   contains

      type(base1) function adda(a, b)
         class(base1), intent(in) :: a
         class(child1), intent(in)  :: b

         adda = base1(10)

      end function

      type(base1) function addb(a, b)
         class(child1), intent(in) :: a
         type(child1), intent(in)  :: b

         addb = base1(10)

      end function

end module

module assignment

   type base2
     integer :: i, j
      contains
         procedure, pass :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(base2) :: child2
   end type

   contains

      subroutine assgn1(a, b)
         class(base2), intent(out) :: a
         class(child2), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(child2), intent(out) :: a
         class(child2), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityInterface003d
   use assignment

   interface assignment(=)
      procedure assgn2
   end interface

end program