!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - polymorphic container with array components
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
      type(base) :: b1(4:6)
   end type

   type, extends(container) :: c_container
      type(child)  :: c1(10:11)
   end type

   contains

      elemental subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in)  :: b

         a%i = b%i + 1

         select type ( a )
            type is ( child )
               select type ( b )
                  type is ( child )
                     a%j = b%j + 1
               end select
         end select

      end subroutine

end module

program genericAssignmentDtIntrinAssgn034
   use m

   class(container), allocatable :: c1, c2
   class(c_container), allocatable :: c3

   allocate ( c1, c2, c3 )

   select type ( g => c1 )
      type is ( container )
         g = container((/ base(1), base(2), base(3) /))
         print *, g

         select type ( c2 )
            type is ( container )
               c2 = c1
               print *, c2
         end select

   end select

   deallocate ( c1, c2 )

   allocate ( c_container :: c2, c1 )

   select type ( c2 )
      type is ( c_container )

         c2 = c_container( (/ base(4), base(5), base(6) /) , (/ child(7,8), child(9,10) /) )
         print *, c2

         select type ( c1 )
            type is ( c_container )
               c1 = c2
               print *, c1
         end select

         select type ( c3 )
            type is ( c_container )
               c3 = c2
               print *, c3
         end select

   end select

end program
