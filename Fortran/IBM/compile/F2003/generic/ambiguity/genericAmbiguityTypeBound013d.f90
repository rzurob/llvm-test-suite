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
!*  DESCRIPTION                : two argument with pass-arg to be first/second arg specified (for generic-name, operator, and assignment tb)
!*                                  - one different independent types, but ambiguous type bound
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
   end type

   type b2
      integer :: i
   end type
   
   type, extends(b2) :: c2
      integer :: j
   end type

   interface print
      module procedure printa
      module procedure printb
   end interface

   contains

      subroutine printa(a, b)
         class(b1), intent(in) :: a
         class(b2),  intent(in) :: b

      end subroutine

      subroutine printb(a, b)
         type(b1), intent(in)  :: a
         type(c2), intent(in) :: b

      end subroutine

end module

module binoperator

   type b11
      integer :: i
      contains
         procedure, pass :: adda
         generic :: operator(+) => adda
   end type

   type b12
      integer :: i
   end type

   type, extends(b12) :: c12
      contains
         procedure, pass(b) :: addb
         generic :: operator(+) => addb
   end type
   contains

      type(b11) function adda(a, b)
         class(b11), intent(in) :: a
         class(b12), intent(in) :: b

         adda = b11(10)

      end function

      type(b11) function addb(a, b)
         class(b11), intent(in) :: a
         class(c12), intent(in) :: b

         addb = b11(20)

      end function

end module

module assignment

   type b12
     integer :: i, j
      contains
         procedure, pass :: assgn1
         
   end type
   
   type, extends(b12) :: c12
      contains
         generic :: assignment(=) => assgn1      
   end type

   type b22
      contains
         procedure, pass(b) :: assgn2
         generic :: assignment(=) => assgn2
   end type

   contains

      subroutine assgn1(a, b)
         class(b12), intent(out) :: a
         type(b22), intent(in)   :: b

      end subroutine

      subroutine assgn2(a, b)
         class(b12), intent(out) :: a
         class(b22), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound013d
end program
