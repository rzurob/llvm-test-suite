!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - for allocatable component
!*                                    - if component of variable is allocated then it's deallocated
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

   type inner
      integer, allocatable :: j
      contains
         final :: innerfinal, inner1final
   end type inner

   type container
      type(inner), allocatable :: inn0
      type(inner), allocatable :: inn1(:)
   end type

   contains

      subroutine innerfinal(a)
         type(inner), intent(inout) :: a

         if ( allocated (a%j) ) deallocate ( a%j )
         print *, 'innerfinal'

      end subroutine

      subroutine inner1final(a)
         type(inner), intent(inout) :: a(:)

         do i = 1, size(a)
            if ( allocated (a(i)%j) ) deallocate ( a(i)%j )
         end do
         print *, 'inner1final'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn016
   use m

   type(container) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3, c2%inn0, c2%inn1(4:10), c3%inn0, c3%inn1(1:-1))

   print *,'set c1:'
   c1 = container( inner(1), (/ inner(2), inner(3) /) )
   print *,'bounds:', lbound(c1%inn1) , ubound(c1%inn1)
   print *,'set c2:'
   c2 = c1
   print *,'bounds:', lbound(c2%inn1) , ubound(c2%inn1)
   print *,'set c3:'
   c3 = c2
   print *,'bounds:', lbound(c3%inn1) , ubound(c3%inn1)

   print *, c1%inn0%j
   print *, c2%inn0%j
   print *, c3%inn0%j

end program
