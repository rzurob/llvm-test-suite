!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - polymorphic container with different types of type components of allocatables
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

module m

   type base
      integer :: i
      contains
         procedure :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child
      integer :: j
   end type

   type container
      type(base), allocatable   :: b1
   end type

   type, extends(container) :: c_container
      type(child), allocatable :: c1
   end type

   contains

      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in)  :: b

         a%i = b%i

         select type ( a )
            type is ( child )
               select type ( b )
                  type is ( child )
                     a%j = b%j
               end select
         end select

         print *, 'bassgn'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn032
   use m

   class(container), allocatable :: c1, c2
   class(c_container), allocatable :: c3

   allocate ( c1, c2, c3 )

   select type ( g => c1 )
      type is ( container )
         g = container(base(100))
         print *, g%b1

         select type ( c2 )
            type is ( container )
               c2 = g
               print *, c2%b1
         end select

         g%b1%i = 101

   end select

   select type ( c2 )
      type is ( container )
         c2 = c1
         print *, c2%b1
   end select

   deallocate ( c1, c2 )

   allocate ( c_container :: c2, c1 )

   select type ( c2 )
      type is ( c_container )

         c2 = c_container( base(200), child(300, 400) )
         print *, c2%b1, c2%c1

         select type ( c1 )
            type is ( c_container )
               c1 = c2
               print *, c1%b1, c1%c1
         end select

         select type ( c3 )
            type is ( c_container )
               c3 = c2
               print *, c3%b1, c3%c1
         end select

   end select

end program
