! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/functional/genericOperatorPrecedence001.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Unary Operators and Binary Operators
!*                                         defined unary+ should be lower precedence than defined *
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
      integer(k1)      i
      contains
         procedure :: unaryplus
         generic :: operator(+) => unaryplus
         procedure :: mul
         generic :: operator(*) => mul
   end type

   contains

   type(base(4)) function unaryplus(a)
      class(base(4)), intent(in) :: a

      if ( a%i < 0 ) then
         unaryplus%i = -1* a%i
      else
      	 unaryplus%i = a%i
      end if

      print *, 'unary+'

   end function

   type(base(4)) function mul(a, b)
      class(base(4)), intent(in) :: a, b
      mul%i = a%i * b%i

      print *, 'binary*'
   end function


end module

program genericOperatorPrecedence001
   use m

   type(base(4)) :: b1, b2, b3, b4

   b1 = +base(4)(1)
   b2 = +base(4)(-2)

   print *, b1%i, b2%i

   b3 = b1 * b2
   b4 = b2 * b3

   print *, b3%i, b4%i

   b1 = base(4)(-1)
   b2 = base(4)(-2)
   b3 = base(4)(-3)
   b4 = base(4)(-4)

   b1 = + b1 * b2
   print *, b1%i

   b3 = + b2 * b3 * b4
   print *, b3%i

end program
