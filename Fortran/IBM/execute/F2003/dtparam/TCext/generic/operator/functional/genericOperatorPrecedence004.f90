! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/functional/genericOperatorPrecedence004.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Unary Operators and Binary Operators
!*                                         defined binary + should be lower precedence than defined *
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
         procedure :: add
         generic :: operator(+) => add
         generic :: operator(*) => add
   end type

   contains

   type(base(20,4)) function add(a,b)
      class(base(*,4)), intent(in) :: a,b

      add%i = a%i + b%i

      print *, add%i, ' = ', a%i,' add ', b%i

   end function


end module

program genericOperatorPrecedence004
   use m

   type(base(20,4)) :: b1, b2, b3, b4

   b1 = base(20,4)(2) + base(20,4)(2)
   b2 = base(20,4)(4) * base(20,4)(2)
   print *, b1%i, b2%i

   b3 = b1 + b2 * b2
   print *, b3%i

   b4 = b3 + b1 * b3 + b2
   print *, b4%i

   b3 = base(20,4)(2) + base(20,4)(4) + base(20,4)(6) * base(20,4)(8) * base(20,4)(10)
   print *, b3%i

   b4 = b1 * base(20,4)(6) + base(20,4)(8) * base(20,4)(10) * base(20,4)(12) + base(20,4)(14)
   print *,  b4%i

end program
