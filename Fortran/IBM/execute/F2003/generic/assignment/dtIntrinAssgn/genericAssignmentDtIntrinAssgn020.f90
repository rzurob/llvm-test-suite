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
!*                                        - base type has a defined assignment
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
         procedure :: btob
         generic :: assignment(=) => btob
   end type

   type, extends(base) :: child
      integer :: j = -999
   end type

   type container
      type(base),  allocatable :: b1
      type(child), allocatable :: c1
   end type

   contains

      subroutine btob(a,b)
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i + 1

         select type ( a )
            type is ( child )
               select type ( b )
                  type is ( child )
                     a%j = b%j + 1
               end select
         end select
         print *, 'btob'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn020
   use m

   type(container) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3
   target :: c1

   c1 = container( base(100), child(200,300) )
   print *, c1%b1, c1%c1

   allocate ( c2, c3 )
   c2 = c1
   print *, c2%b1, c2%c1

   c3 = c2
   print *, c3%b1, c3%c1

   c3 = container( base(50), child(51, 52) )
   print *, c3%b1, c3%c1

   c2 => c1
   c2 = c3

   print *, c1%b1, c1%c1
   print *, c2%b1, c2%c1

   c1%b1%i = -999

   c1%b1 = c2%c1%base
   print *, c1%b1, c1%c1

end program
