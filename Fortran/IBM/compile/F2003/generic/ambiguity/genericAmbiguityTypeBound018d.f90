!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : With Class Hierarchy
!*
!*                                     ( Base )
!*                                      /    \
!*                                 (Child1) (Child2)
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
         procedure, nopass :: twoargs1
         procedure, nopass :: twoargs2

         generic :: print => twoargs1, twoargs2

   end type

   type, extends(b1) :: c1
   end type

   type, extends(b1) :: c2
   end type

   contains

      subroutine twoargs1(a, b)
         class(c1), intent(in) :: a
         class(b1), intent(in) :: b

      end subroutine

      subroutine twoargs2(a, b)
         class(b1), intent(in) :: a
         class(c2), intent(in) :: b

      end subroutine

end module

module binoperator

   type b11
      integer :: i
      contains
         procedure, pass :: adda
         generic :: operator(+) => adda
   end type

   type, extends(b11) :: c11
      contains
         procedure, pass :: addb
         generic :: operator(+) => addb
   end type

   type, extends(b11) :: c12
   end type

   contains

      type(b11) function adda(a, b)
         class(b11), intent(in) :: a
         class(c12), intent(in) :: b

         adda = b11(10)

      end function

      type(b11) function addb(a, b)
         class(c11), intent(in) :: a
         class(b11), intent(in) :: b

         addb = b11(10)

      end function

end module

module assignment

   type b21
      integer :: i
   end type

   type, extends(b21) :: c21
      contains
         procedure, pass(a) :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(b21) :: c22
      contains
         procedure, pass(b) :: assgn2
         generic :: assignment(=) => assgn2
   end type

   contains

      subroutine assgn1(a, b)
         class(c21), intent(out) :: a
         class(b21), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(b21), intent(out) :: a
         class(c22), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound018d
end program
