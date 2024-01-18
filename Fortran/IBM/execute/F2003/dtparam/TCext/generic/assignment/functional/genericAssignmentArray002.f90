! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/assignment/functional/genericAssignmentArray002.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: operands with non-poly assumed shape array
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i =0
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(4)), intent(out) :: a
         type(base(4)), intent(in) :: b(:)

         do i = 1, size(b)
            a%i = a%i + b(i)%i
         end do

         print *, 'bassgn'

      end subroutine

end module


program genericAssignmentArray002
   use m

   type(base(4)) :: b1
   type(base(4)), allocatable :: b2(:)
   type(base(4)), pointer :: b3(:)

   allocate ( b2(5), b3(7) )
   b2 = (/ (base(4)(i),i=10,14) /)

   b3 = (/ (base(4)(i),i=20,26) /)

   b1 = b2
   print *, b1

   b1 = b3
   print *, b1

   b1 = b3(2:)
   print *, b1

   b1 = b2(1:5:2)
   print *, b1

   b1 = b3((/1,7,2,6,3,5,4/))
   print *, b1

end program
