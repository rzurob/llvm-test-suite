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
!*  DESCRIPTION                : With Class Hierarchy
!*
!*                                      ( Base )
!*                                      /        \
!*                                 (Child1)   (Child2)
!*                                     |         |
!*                                 (Gen31)    (Gen32)
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
         generic :: twoargs => twoargs1, twoargs2
   end type

   type, extends(b1) :: c1
   end type

   type, extends(b1) :: c2
   end type

   type, extends(c1) :: g1
   end type

   type, extends(c2) :: g2
      contains
         procedure, nopass :: twoargs3
         generic :: twoargs => twoargs3
   end type

   contains

      subroutine twoargs1(a, b)
         class(b1), intent(in) :: a
         type(b1), intent(in) :: b

      end subroutine

      subroutine twoargs2(a, b)
         class(b1), intent(in) :: a
         class(c1), intent(in) :: b

      end subroutine

      subroutine twoargs3(a, b)
         type(c1), intent(in) :: a
         class(b1), intent(in) :: b

      end subroutine

end module

module unoperator

   type b11
      integer :: i
      contains
         procedure, pass :: addb
         generic :: operator(.myun.) => addb
   end type

   type, extends(b11) :: c11
   end type

   type, extends(b11) :: c12
   end type

   type, extends(c11) :: g11
   end type

   type, extends(c12) :: g12
      contains
         procedure, pass :: addc
         generic :: operator(.myun.) => addc
   end type

   contains

      type(b11) function addb(a)
         class(b11), intent(in) :: a

         addb = b11(10)

      end function

      type(b11) function addc(b)
         class(g12), intent(in) :: b

         addc = b11(10)

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
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn2
   end type

   type, extends(c21) :: g21

   end type

   type, extends(c22) :: g22
   end type

   contains

      subroutine assgn1(a, b)
         type(g22), intent(out) :: a
         class(c21), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(c22), intent(out) :: a
         type(c21), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound023d
end program
