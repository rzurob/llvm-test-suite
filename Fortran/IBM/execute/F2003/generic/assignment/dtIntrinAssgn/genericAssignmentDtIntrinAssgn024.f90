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
!*                                 - for allocatable component
!*                                    - use defined assignment if the "declared"
!*                                      type of the component has a type-bound
!*                                      defined assignment consistent with the components
!*                                        - derived type does not have generic tb, but extended type does
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
   end type

   type, extends(base) :: child
      integer :: j
      contains
         generic :: assignment(=) => bassgn
   end type

   type container
      class(base), allocatable :: b1
      class(child), allocatable :: c1
   end type

   contains

      subroutine bassgn ( a, b )
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

         print *, 'bassgn'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn024
   use m

   type(container) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3 )

   c1 = container ( base(1), child(1,2) )
   c2 = c1
   c3 = c2

   print *, c1%b1%i, c1%c1%i, c1%c1%j
   print *, c2%b1%i, c2%c1%i, c2%c1%j
   print *, c3%b1%i, c3%c1%i, c3%c1%j

   c2 = container ( child(1,1), child(1,2) )
   c1 = c2
   c3 = c1

   select type ( g => c1%b1 )
      type is ( child )
         print *, g, c1%c1%i, c1%c1%j
   end select

   select type ( g => c2%b1 )
      type is ( child )
         print *, g, c2%c1%i, c2%c1%j
   end select

   select type ( g => c3%b1 )
      type is ( child )
         print *, g, c3%c1%i, c3%c1%j
   end select
   
end program
