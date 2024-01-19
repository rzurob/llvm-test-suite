! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorScalar011.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar function return with pointer attribute
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
         generic :: operator (-) => sub1, sub2
         procedure, private :: sub1
         procedure, private :: sub2
   end type

   contains

   function sub1 (a, b)
      class(base(*,4)), intent(in) :: a, b

      type(base(:,4)), pointer :: sub1
      allocate ( sub1, source = base(20,4) ( a%i - b%i ) )

   end function sub1

   function sub2 (a, b)
      class(base(*,4)), intent(in) :: a
      integer, intent(in) :: b

      type(base(:,4)), pointer :: sub2
      allocate ( sub2, source = base(20,4) ( a%i - b ) )

   end function sub2

end module

program genericOperatorScalar011
   use m

   class(base(:,4)), pointer :: b1
   type(base(:,4)) , pointer :: b2

   b1 => base(20,4)(10) - base(20,4)(5)
   b2 => b1 - base(20,4)(-15)

   if ( b1%i /= 5 ) error stop 1_4
   if ( b2%i /= 20 ) error stop 2_4

   b1 => b2 - b1
   b2 => b1 - base(20,4)(-100)

   if ( b1%i /= 15 )  error stop 3_4
   if ( b2%i /= 115 ) error stop 4_4

   if ( .not. associated( b2 - b1 ) ) error stop 5_4

   b1 => b2 - 15
   b2 => b1 - base(20,4)(-100)

   if ( b1%i /= 100 )  error stop 8_4
   if ( b2%i /= 200 )  error stop 9_4

   if ( .not. associated( b2 - b1 ) ) error stop 10_4

end program
