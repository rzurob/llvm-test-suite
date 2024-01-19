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
!*                                    when components that has type hierarchy
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

   type com
      integer :: i
      contains
         generic :: assignment(=) => c1assgn
         procedure, pass :: c1assgn
   end type

   type, extends(com) :: comchild
      integer :: j
   end type

   contains

      subroutine c1assgn ( a, b )
         class(com), intent(out) :: a
         class(com), intent(in) :: b

         a%i = b%i
         print *, 'c1assgn'

         select type ( a )
            type is ( comchild )
               select type ( b )
                  type is ( comchild )
                     print *, 'child'
                     a%j = b%j
               end select
         end select

      end subroutine

end module

module m1
   use m

   type base
      integer :: x
      type(com) :: c1
      type(com), pointer :: c2
      type(comchild) :: cc1
      type(comchild), pointer :: cc2
   end type

end module

program genericAssignmentDtIntrinAssgn006
   use m1

   type(base) :: b1
   type(base), pointer :: b2
   type(base), allocatable :: b3

   type(com), target :: c1 = com(200)
   type(comchild), target :: cc1 = comchild(500,600)

   b1 = base ( 100, com(100), c1, comchild(300,400), null() )
   print *, b1%x, b1%c1, b1%c2, b1%cc1, associated (b1%cc2)

   allocate ( b2, b3 )
   b2 = b1
   print *, b2%x, b2%c1, b2%c2, b2%cc1, associated (b2%cc2)

   b2%cc2 => cc1
   print *, associated (b2%cc2), b2%cc2

   b3 = b2
   print *, b3%x, b3%c1, b3%c2, b3%cc1, b3%cc2, associated ( b3%c2 , b2%c2 ), associated ( b3%cc2 , b2%cc2 )

end program
