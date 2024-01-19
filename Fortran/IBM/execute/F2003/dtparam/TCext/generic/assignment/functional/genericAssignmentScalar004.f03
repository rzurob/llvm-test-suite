! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar004.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: operands with non-poly scalar with allocatable components
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
      integer, kind            :: k1
      integer(k1), allocatable :: i
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(4)), intent(out) :: a
         type(base(4)), intent(in) :: b

         if ( .not. allocated ( a%i ) ) allocate ( a%i )

         a%i = b%i
         print *, a%i, '=', b%i

      end subroutine

end module


program genericAssignmentScalar004
   use m

   type(base(4)), pointer :: b1
   type(base(4)), allocatable :: b2
   type(base(4)), target :: b3

   allocate ( b1, source = base(4)( 20 ) )
   allocate ( b2, source = base(4)( 30 ) )

   b3 = base(4)( 40 )
   if ( b3%i /= 40 ) error stop 1_4

   b1 = b2
   if ( b1%i /= 30 ) error stop 2_4

   b1 = b3
   if ( b1%i /= 40 ) error stop 3_4

   b2 = b3
   if ( b2%i /= 40 ) error stop 4_4

   b3 = b2
   if ( b3%i /= 40 ) error stop 5_4

end program
