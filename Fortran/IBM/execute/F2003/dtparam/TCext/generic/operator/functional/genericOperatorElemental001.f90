! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorElemental001.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: with elemental function
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
      integer(k1)   :: x = -999
      contains
         generic :: operator(*) => mul
         procedure :: mul
   end type

   contains

      elemental type(base(20,4)) function mul ( a, b )
         class(base(*,4)), intent(in) :: a,b

         mul%x = a%x * b%x

      end function

end module

program genericOperatorElemental001
   use m

   type(base(20,4)) :: b1, b11(3)
   type(base(:,4)), pointer     :: b2(:), b3(:)

   b1 = base(20,4)(5)
   b11 = (/ base(20,4)(1), base(20,4)(2), base(20,4)(3) /)

   b11 = b11 * b1
   print *, b11%x

   b11 = b11 * b11
   print *, b11%x

   allocate ( b2(5), source = (/ ( base(20,4)(i), i=-2,2 ) /) )
   allocate ( b3(5), source = (/ ( base(20,4)(i), i=2,-2,-1 ) /) )

   b11 = b2(1:5:2) * b3((/1,3,5/))
   print *, b11%x

   b11 = b3((/1,2,5/)) * b2(5)
   print *, b11%x

end program
