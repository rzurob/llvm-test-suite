! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar002.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: operands with non-poly scalar (with pointer, allocatable attribute)
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type


   contains

      subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         type(base(*,4)), intent(in) :: b

         a%i = b%i
         print *, a%i, '=', b%i

      end subroutine

end module


program genericAssignmentScalar002
   use m

   type(base(:,4)), pointer :: b1
   type(base(:,4)), allocatable :: b2

   type(base(20,4)), target :: b3

   allocate ( b1, source = base(20,4)(10) )
   allocate ( b2, source = b1 )

   if ( ( b1%i /= 10 ) .or. ( b2%i /= 10 ) ) error stop 1_4

   b3%i = 999
   b1 => b3

   b1 = b2

   if ( ( b1%i /= 10 ) .or. ( b2%i /= 10 ) .or. ( b3%i /= 10 ) ) error stop 2_4

   b3 = base(20,4)(100)
   b2 = b1

   if ( ( b1%i /= 100 ) .or. ( b2%i /= 100 ) .or. ( b3%i /= 100 ) ) error stop 3_4

end program
