!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: operands with poly scalar with overridding specific typebound
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

   type base
      integer :: i = -999
      contains
         procedure, pass :: assgn => ba
         generic :: assignment(=) => assgn
   end type


   type, extends(base) :: child
      integer :: j = -999
      contains
         procedure, pass :: assgn => ca
   end type

   contains

      subroutine ba ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i
         print *, 'ba'

      end subroutine

      subroutine ca ( a, b )
         class(child), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i

         print *, 'ca'

         select type ( b )
            type is ( child )
               a%j = b%j
         end select

      end subroutine

end module


program genericAssignmentScalar008
   use m

   class(base), pointer :: b1
   class(base), allocatable :: b2
   type(child) :: c1
   class(child), pointer :: c2

   allocate ( b1, source = base(10) )
   allocate ( b2 )

   b2 = b1
   c1 = b1

   print *, b2%i
   print *, c1%i, c1%j

   allocate ( c2 )

   b1 = c1
   c2 = child( 10 , 20 )
   c1 = c2

   print *, b1%i
   print *, c2%i, c2%j
   print *, c1%i, c1%j

   deallocate ( b1, b2 )

   allocate ( b1, source = child() )
   allocate ( b2, source = base ( 100 ) )

   b1 = b2
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select

   b1 = c1
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
   end select


end program
