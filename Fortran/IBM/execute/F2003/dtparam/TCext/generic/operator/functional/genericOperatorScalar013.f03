! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorScalar013.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar function return with allocatable
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
         generic :: operator (+) => add1, add2
         procedure, private :: add1
         procedure :: add2
   end type

   contains

   function add1 (a, b)
      class(base(*,4)), intent(in) :: a, b

      type(base(:,4)), allocatable :: add1
      allocate ( add1, source = base(20,4) ( a%i + b%i ) )

   end function add1

   function add2 (a, b)
      class(base(*,4)), intent(in) :: a
      integer, intent(in) :: b

      type(base(:,4)), allocatable :: add2
      allocate ( add2, source = base(20,4) ( a%i + b ) )

   end function add2

end module

program genericOperatorScalar013
   use m

   type(base(:,4)), allocatable :: b1, b2
   class(base(:,4)), pointer :: b3

   allocate ( b1, source = base(20,4)(10) + base(20,4)(20) )
   allocate ( base(20,4):: b2 )
   b2 = b1 + ( base(20,4)(-15) )

   if ( b1%i /= 30 ) error stop 1_4
   if ( b2%i /= 15 ) error stop 2_4

   allocate ( b3, source = b1 + b2 )
   if ( b3%i /= 45 ) error stop 3_4

   deallocate ( b1, b2, b3 )

   allocate ( b1, source = base(20,4) ( 5 ) + 15 )
   allocate ( b2, source = b1 + (-10) )

   allocate ( b3, source =  b1 + b2 + 10 + 20 )

   if ( b1%i /= 20 ) error stop 4_4
   if ( b2%i /= 10 ) error stop 5_4
   if ( b3%i /= 60 ) error stop 6_4

end program