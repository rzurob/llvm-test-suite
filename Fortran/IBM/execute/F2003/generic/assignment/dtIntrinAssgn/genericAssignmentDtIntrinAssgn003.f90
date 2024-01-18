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
!*                                  - A derived-type intrinsic assignment is performed as if each
!*                                    component of variable were assignmed from the corresponding of
!*                                    expr using pointer assignment for each pointer component.
!*                                     - all pointer components should not invoke generic assignment type bound
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

   type base
      integer, pointer :: x => null()
      type(com1), pointer :: c1
      type(com2), pointer :: c2
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

program genericAssignmentDtIntrinAssgn003
   use m

   type(base) :: b1
   type(base), allocatable :: b2
   type(base), pointer :: b3

   type(com1), target :: c1 = com1(2006)
   type(com2), target :: c2 = com2('ibm')
   integer, target :: i1 = 1001

   b1 = base( null(), c1, c2 )
   print *, associated(b1%x), b1%c1, b1%c2

   allocate ( b2 )
   b2 = base ( i1, b1%c1, c2 )
   print *, associated(b2%x), b2%c1, b2%c2
   print *, associated( b1%x, b2%x ), associated( b1%c1, b2%c1 ), associated( b1%c2, b2%c2 ), associated( b2%c1, c1 ), associated( b2%c2, c2 )

   b2 = b1
   print *, associated(b2%x), b2%c1, b2%c2
   print *, associated( b1%x, b2%x ), associated( b1%c1, b2%c1 ), associated( b1%c2, b2%c2 ), associated( b2%c1, c1 ), associated( b2%c2, c2 )

   b2%x => i1

   allocate ( b3 )
   b3 = b2
   print *, associated(b3%x), b3%c1, b3%c2
   print *, associated( b3%x, b2%x ), associated( b3%c1, b2%c1 ), associated( b3%c2, b2%c2 ), associated( b3%c1, c1 ), associated( b3%c2, c2 )

end program
