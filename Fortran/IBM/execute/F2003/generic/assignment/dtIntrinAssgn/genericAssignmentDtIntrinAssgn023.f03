!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - for allocatable component
!*                                    - use defined assignment if the "declared"
!*                                      type of the component has a type-bound
!*                                      defined assignment consistent with the components
!*                                        - component is unlimited polymorphic
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

   type container
      class(*), allocatable :: u1
   end type

   type base
      integer :: i
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child
      integer :: j
   end type

   contains

      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         error stop 1_4

      end subroutine

end module

program genericAssignmentDtIntrinAssgn023
   use m

   type(container) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3 )

   c1 = container ( 10_8 )
   c2 = c1
   c3 = c2

   select type ( g => c1%u1 )
      type is ( integer(8) )
         print *, g
   end select

   select type ( g => c2%u1 )
      type is ( integer(8) )
         print *, g
   end select

   select type ( g => c3%u1 )
      type is ( integer(8) )
         print *, g
   end select

   c2 = container(base(20))
   c1 = c2
   c3 = c1

   select type ( g => c1%u1 )
      type is ( base )
         print *, g
   end select

   select type ( g => c2%u1 )
      type is ( base )
         print *, g
   end select

   select type ( g => c3%u1 )
      type is ( base )
         print *, g
   end select

   c3 = container(child(30,40))
   c1 = c3
   c2 = c3

   select type ( g => c1%u1 )
      type is ( child )
         print *, g
   end select

   select type ( g => c2%u1 )
      type is ( child )
         print *, g
   end select

   select type ( g => c3%u1 )
      type is ( child )
         print *, g
   end select

end program
