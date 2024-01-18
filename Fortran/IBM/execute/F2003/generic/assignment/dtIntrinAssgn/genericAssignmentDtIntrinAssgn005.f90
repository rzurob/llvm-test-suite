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
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                  - Perform generic type bound assignment for the type
!*                                    component when it is declared for the type
!*                                    when there is type hierarchy on container type
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

      subroutine c1assgn ( a, b )
         class(com1), intent(out) :: a
         class(com1), intent(in) :: b

         a%i = b%i
         print *, 'c1assgn'

      end subroutine

      subroutine c2assgn ( a, b )
         class(com2), intent(out) :: a
         class(com2), intent(in) :: b

        a%c = b%c
         print *, 'c2assgn'

      end subroutine

end module

module m1
   use m

   type base
      integer :: x
      type(com1) :: c11
      type(com2) :: c21
   end type

   type, extends(base) :: child
      type(com1) :: c12
      type(com2) :: c22
   end type

end module

program genericAssignmentDtIntrinAssgn005
   use m1

   type(base) :: b1
   type(base), allocatable :: b2
   type(child) :: c1
   type(child), pointer :: c2

   allocate ( b2, c2 )

   b1 = base(100, com1(100), com2('aaa'))
   print *, b1
   b2 = base(200, com1(200), com2('bbb'))
   print *, b2

   c1 = child(1000, com1(1000), com2('ccc'), com1(2000), com2('ddd') )
   print *, c1

   c2 = child(3000, com1(3000), com2('eee'), com1(4000), com2('fff') )
   print *, c2

   b1 = c1%base
   print *, b1

   b2 = c2%base
   print *, b2

   c1 = c2
   print *, c1

end program
