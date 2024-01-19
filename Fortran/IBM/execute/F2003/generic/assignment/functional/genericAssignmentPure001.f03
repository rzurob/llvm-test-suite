!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: pure subroutine with scalar and class hierarchy
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
      integer :: i
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child
      integer :: j
      contains
         procedure, pass :: bassgn => cassgn
         procedure, pass :: cassgnint
         generic :: assignment(=) => cassgnint
   end type

   contains

      pure subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i + 1

      end subroutine

      pure subroutine cassgn ( a, b )
         class(child), intent(out) :: a
         class(base), intent(in)   :: b

         select type ( b )
            type is ( base )
               a%i = b%i + 1
            type is ( child )
               a%base = b%base
               a%j = b%j + 1
         end select

      end subroutine

      pure subroutine cassgnint ( a, b )
         class(child), intent(out) :: a
         integer, intent(in) :: b

         a%i = b + 1
         a%j = b + 1

      end subroutine

end module

program genericAssignmentPure001
   use m

   class(base), pointer  :: b1
   class(child), pointer :: c1

   type(base) :: b2
   type(child) :: c2

   allocate ( b1, c1 )

   b2 = base(100)
   c2 = child(1000, 2000)

   print *, b2
   print *, c2

   b1 = b2
   c1 = c2

   print *, b1%i
   print *, c1%i, c1%j

   c1 = -1000
   c2 = -1000

   print *, c1%i, c1%j
   print *, c2%i, c2%j

   deallocate ( b1 )
   allocate ( child :: b1 )

   b1 = c1
   select type ( b1 )
      type is ( child )
         print *, b1%i, b1%j
         b1 = 1000
         print *, b1%i, b1%j
   end select

end program
