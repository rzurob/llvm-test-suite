!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - container type contains a derived type
!*                                     - container is an array object, and component
!*                                       is either scalar and array object that has
!*                                       elemental UD assignment
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
      integer :: j
      contains
         procedure :: elemA
         generic :: assignment(=) => elemA
   end type inner

   type container
      type(inner) :: i0
      type(inner) :: i1(3)
   end type

   contains

      elemental subroutine elemA(a, b)
         class(inner), intent(out) :: a
         class(inner), intent(in)  :: b

         a%j = b%j + 1

      end subroutine

end module

program genericAssignmentDtIntrinAssgn015
   use m

   type(container) :: a(3), b(:), c(:)

   pointer :: b
   allocatable :: c

   allocate ( b(3), c(3) )

   a = container( i1= (/inner(2), inner(3), inner(4) /), i0= inner(1) )
   print *,a

   b = (/ container( i0= inner(1), i1= (/inner(2), inner(3), inner(4) /) ), &
          ( container( i0= inner(i), i1= (/inner(i+1), inner(i+2), inner(i+3) /) ), i = 5,9,4) /)
   print *, b

   c = b((/2,3,1/))
   print *, c

end program
