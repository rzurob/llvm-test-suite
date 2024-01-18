!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                  - Perform generic type bound assignment for the type
!*                                    component when it is declared for the type
!*                                    where the component is an array, and elemental generic tb assignment is defined
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

   type com1
      integer :: i
      contains
         generic :: assignment(=) => c1assgn
         procedure, pass :: c1assgn
   end type

   type com2
      character(3) :: c
      contains
         generic :: assignment(=) => c2assgn
         procedure, pass :: c2assgn
   end type

   contains

      elemental subroutine c1assgn ( a, b )
         class(com1), intent(out) :: a
         class(com1), intent(in) :: b

         a%i = b%i + 1

      end subroutine

      elemental subroutine c2assgn ( a, b )
         class(com2), intent(out) :: a
         class(com2), intent(in) :: b

         a%c(1:3) = b%c(1:2) // 'x'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn004
   use m

   type base
      integer :: x
      type(com1) :: c1(3)
      type(com2) :: c2(2,2)
   end type

   type(base) :: b1
   type(base), allocatable :: b2
   type(base), pointer :: b3, b4

   b1 = base ( 100, (/ com1(10), com1(20), com1(30)  /), reshape ( source = (/ com2('abc'), com2('def'), com2('ghi'), com2('jkl') /), shape =(/ 2, 2 /) ) )
   print *, b1

   allocate ( b2 )

   b2 = b1
   print *, b2

   allocate ( b4 )
   b3 => b4

   b3 = b2

   print *, b3
   print *, b4

end program
