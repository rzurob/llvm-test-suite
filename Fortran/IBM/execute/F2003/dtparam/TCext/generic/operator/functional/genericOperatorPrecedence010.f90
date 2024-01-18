! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/functional/genericOperatorPrecedence010.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: User Defined Operator (Unary and Binary)
!*                                         defined unary should be higher precedence than defined ** or *
!*                                         defined binary should be lower precedence than defined ** or *
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
      integer(k1)      i
      contains
         procedure :: myu
         generic :: operator(.u.) => myu
         procedure :: myb
         generic :: operator(.b.) => myb
         procedure :: pow
         generic :: operator(**) => pow
         procedure :: mul
         generic :: operator(*) => mul
   end type

   contains

   type(base(20,4)) function myu ( a )
      class(base(*,4)), intent(in) :: a

      myu = a
      myu%i = myu%i * (-1)

      print *, 'myu'

   end function

   type(base(20,4)) function myb ( a, b )
      class(base(*,4)), intent(in) :: a, b

      myb%i = a%i + b%i

      print *, 'myb'

   end function

   type(base(20,4)) function pow ( a, b )
      class(base(*,4)), intent(in) :: a, b

      pow%i = a%i ** b%i

      print *, 'pow'

   end function

   type(base(20,4)) function mul ( a, b )
      class(base(*,4)), intent(in) :: a, b

      mul%i = a%i * b%i

      print *, 'mul'

   end function

end module

program genericOperatorPrecedence010
   use m

   type(base(20,4)) :: b1, b2, b3, b4

   b1 = .u. base(20,4)(-1)
   b2 = b1 .b. base(20,4)(1)
   b3 = b1 * b2
   b4 = b2 ** b3

   print *, b1, b2, b3, b4

   b1 = .u. b1 .b. b2 * b3 ** b4
   print *, b1

   b2 = b1 * b2 ** b2 .b. b3 .b. .u. b4
   print *, b2

   b3 = b4 ** b3 .b. b2 .b. .u. b1
   print *, b3

   b4 = .u. b1 .b. b2 * b3 * b4
   print *, b4

end program
