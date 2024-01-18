!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : With Class Hierarchy
!*
!*                                               ( Base )
!*                                      /       /         \        \
!*                                 (Child1) (Child2)    (Child3) (Child4)
!*
!*                               - with more child types
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

         generic :: twoarga => twoargs1, twoargs2, twoargs3, twoargs4

         procedure, nopass :: twoargs1
         procedure, nopass :: twoargs2
         procedure, nopass :: twoargs3
         procedure, nopass :: twoargs4
   end type

   type, extends(b1) :: c1
   end type

   type, extends(b1) :: c2
   end type

   type, extends(b1) :: c3
   end type

   type, extends(b1) :: c4
   end type

   contains

      subroutine twoargs1(a, b)
         class(b1), intent(in) :: a
         class(c1), intent(in) :: b

      end subroutine

      subroutine twoargs2(a, b)
         class(c1), intent(in) :: a
         class(c2), intent(in) :: b

      end subroutine

      subroutine twoargs3(a, b)
         class(c3), intent(in) :: a
         class(c4), intent(in) :: b

      end subroutine

      subroutine twoargs4(a, b)
         class(c4), intent(in) :: a
         class(c1), intent(in) :: b

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
      contains
         procedure, pass :: addc
         generic :: operator(+) => addc
   end type

   type, extends(b11) :: c13
      contains
         procedure, pass :: addd
         generic :: operator(+) => addd
   end type

   type, extends(b11) :: c14
      contains
         procedure, pass :: adde
         generic :: operator(+) => adde
   end type

   contains

      type(b11) function adda(a, b)
         class(b11), intent(in) :: a
         class(c11), intent(in) :: b

         adda = b11(10)

      end function

      type(b11) function addb(a, b)
         class(c11), intent(in) :: a
         class(c12), intent(in) :: b

         addb = b11(10)

      end function

      type(b11) function addc(a, b)
         class(c12), intent(in) :: a
         class(c13), intent(in) :: b

         addc = b11(10)

      end function

      type(b11) function addd(a, b)
         class(c13), intent(in) :: a
         class(c11), intent(in) :: b

         addd = b11(10)

      end function

      type(b11) function adde(a, b)
         class(c14), intent(in) :: a
         class(c12), intent(in) :: b

         adde = b11(10)

      end function

end module

module assignment

   type b21
      integer :: i
   end type

   type, extends(b21) :: c21
      contains
         procedure, pass(b) :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(b21) :: c22
      contains
         procedure, pass(a) :: assgn2
         generic :: assignment(=) => assgn2
   end type

   type, extends(b21) :: c23
      contains
         procedure, pass(a) :: assgn3
         generic :: assignment(=) => assgn3
   end type

   type, extends(b21) :: c24
      contains
         procedure, pass(b) :: assgn4
         generic :: assignment(=) => assgn4
   end type

   contains

      subroutine assgn1(a, b)
         class(c21), intent(out) :: a
         class(c21), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(c22), intent(out) :: a
         class(c22), intent(in)  :: b

      end subroutine

      subroutine assgn3(a, b)
         class(c23), intent(out) :: a
         class(c24), intent(in)  :: b

      end subroutine

      subroutine assgn4(a, b)
         class(b21), intent(out) :: a
         class(c24), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound021d
end program
