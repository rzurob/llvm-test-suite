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
!*                                    - if expr's component is unallocated, then variable's component is also unallocated
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

program genericAssignmentDtIntrinAssgn017
   use m

   type(container) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3, c2%inn1(2:3) )

   print *,'set c1:'
   c1 = container( inner(1), (/ (inner(j), j =6,0) /) )
   print *,'bounds:', lbound(c1%inn1) , ubound(c1%inn1), allocated(c1%inn0), allocated(c1%inn1)
   print *,'set c2:'
   c1 = c2
   print *,'bounds:', lbound(c1%inn1) , ubound(c1%inn1), allocated(c1%inn0), allocated(c1%inn1)
   print *,'set c3:'
   c1 = c3
   print *,'bounds:', allocated(c1%inn0), allocated(c1%inn1)

end program
