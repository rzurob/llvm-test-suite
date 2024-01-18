!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: operands with non-poly assumed size array
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
      integer :: i =0
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         type(base), intent(in) :: b(*)

         do i = 1, 5
            a%i = a%i + b(i)%i
         end do

         print *, 'bassgn'

      end subroutine

end module


program genericAssignmentArray003
   use m

   type(base) :: b1
   type(base), allocatable :: b2(:)
   type(base), pointer :: b3(:)

   allocate ( b2(10), b3(7) )
   b2 = (/ (base(i),i=50,54), (base(0), i=1,5) /)

   b3 = (/ (base(i),i=70,76) /)

   b1 = b2
   print *, b1

   b1 = b3
   print *, b1

   b1 = b3(2:)
   print *, b1

   b1 = b2(1:10:2)
   print *, b1

   b1 = b3((/1,7,2,6,3,5,4/))
   print *, b1

end program
