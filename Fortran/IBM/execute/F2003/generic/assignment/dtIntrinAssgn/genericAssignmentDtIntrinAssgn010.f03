!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                      - try a derived type containing an derived type scalar and 1d array, but only
!*                                        scalar intrinic assignment is defined for generic type bound, 1d array assignment
!*                                        is defined with interface
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

   type container
      type(base) :: b1 ! scalar
      type(base) :: b2(5) ! array
   end type

   interface assignment(=)
      module procedure barrayassgn
   end interface

   contains

      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i
         print *, 'bassgn'

      end subroutine

      subroutine barrayassgn ( a, b )
         class(base), intent(out) :: a(:)
         class(base), intent(in) :: b(:)

         if ( size ( a ) /= size ( b ) ) error stop 1_4

         do j = 1, size ( a )
            a(j)%i = b(j)%i
         end do

         print *, 'barrayassgn'

      end subroutine

end module

program genericAssignmentDtIntrinAssgn010
   use m

   type(container) :: c1, c2
   type(container), allocatable :: c3
   type(base) :: b1(5) = (/ (base(i), i = 5, 1, -1) /)

   c1 = container( base(1), (/ ( base(i),i = 2, 6 ) /) )
   c2 = c1

   print *, c1
   print *, c2

   allocate ( c3 )
   c3 = c2

   print *, c3

   c2%b2 = c1%b2
   print *, c2%b2

   c3%b2 = b1
   print *, c3%b2

end program
