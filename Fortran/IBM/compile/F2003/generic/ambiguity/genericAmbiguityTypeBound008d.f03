!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : two argument with pass-arg to be first arg specified (for generic-name, operator, and assignment tb)
!*                                  - one arg polymorphic, the other polymorphic extended type
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
      integer :: i
      contains
         procedure, pass :: printa
         generic :: print => printa
   end type

   type, extends(base) :: child
      integer :: j
      contains
         procedure, pass :: printb
         generic :: print => printb
   end type

   contains

      subroutine printa(a, b)
         class(base), intent(in) :: a
         class(base), intent(in) :: b


      end subroutine

      subroutine printb(a, b)
         class(child), intent(in) :: a
         class(child), intent(in) :: b


      end subroutine

end module

module binoperator

   type base1
      integer :: i
      contains
         procedure, pass :: adda
         generic :: operator(+) => adda
   end type

   type, extends(base1) :: child1
      integer :: j
      contains
         procedure, pass :: addb
         generic :: operator(+) => addb
   end type

   contains

      type(base1) function adda(a, b)
         class(base1), intent(in) :: a
         class(base1), intent(in)  :: b

         adda = base1(10)

      end function

      type(base1) function addb(a, b)
         class(child1), intent(in) :: a
         class(child1), intent(in)  :: b

         addb = base1(20)

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
      contains
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn2
   end type

   contains

      subroutine assgn1(a, b)
         class(base2), intent(out) :: a
         class(base2), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(child2), intent(out) :: a
         type(child2), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound008d
end program
