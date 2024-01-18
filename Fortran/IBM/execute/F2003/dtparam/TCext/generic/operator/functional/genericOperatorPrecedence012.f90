! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/functional/genericOperatorPrecedence012.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: User Defined Operator (Unary and Binary)
!*                                         UD Unary and UD Binary of the same name,
!*                                         Binary should have lower precedence than unary
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
         procedure :: myb
         generic :: operator(.ub.) => myu, myb
   end type

   contains

   type(base(20,4)) function myu (a)
      class(base(*,4)), intent(in) :: a

      myu%i = a%i + 1
      print *, 'myu:', myu, '=', a%i, '+ 1'

   end function

   type(base(20,4)) function myb (a,b)
      class(base(*,4)), intent(in) :: a, b

      myb%i = a%i + b%i
      print *, 'myb:', myb, '=', a%i, '+', b%i

   end function

end module

program genericOperatorPrecedence012
   use m

   type (base(20,4)) :: b1, b2, b3, b4

   b1 = base(20,4)(10)
   b2 = .ub. b1
   b3 = b1 .ub. b2
   b4 = b1 .ub. b2 .ub. b3

   print *, b1, b2, b3, b4

   b1 = .ub. b1 .ub. b2 .ub. .ub. b3
   b2 = .ub. b2 .ub. b2 .ub. .ub. b2

   print *, b1, b2

end program
