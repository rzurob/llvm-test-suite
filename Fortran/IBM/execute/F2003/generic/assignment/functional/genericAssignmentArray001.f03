!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: operands with non-poly explicit shape array
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
         type(base), intent(in) :: b(5)

         do i = 1, size(b)
            a%i = a%i + b(i)%i
         end do

         print *, 'bassgn'

      end subroutine

end module


program genericAssignmentArray001
   use m

   type(base) :: b1
   type(base), allocatable :: b2(:)
   type(base), pointer :: b3(:)

   allocate ( b2(5), b3(7) )
   b2 = (/ (base(i),i=10,14) /)

   b3 = (/ (base(i),i=20,26) /)

   b1 = b2
   print *, b1

   b1 = b3
   print *, b1

   b1 = b3(2:6)
   print *, b1

end program