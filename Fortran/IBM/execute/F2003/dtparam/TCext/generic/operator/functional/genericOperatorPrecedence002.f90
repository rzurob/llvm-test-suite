! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/functional/genericOperatorPrecedence002.f
! opt variations: -qnol

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: Unary Operators and Binary Operators
!*                                         defined ** should be higher precedence than defined /
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
         procedure :: pow
         generic :: operator(/) => div
         procedure :: div
         generic :: operator(**) => pow
   end type

   contains

   type(base(20,4)) function pow(a,b)
      class(base(*,4)), intent(in) :: a,b

      pow%i = a%i ** b%i

      print *, 'pow'

   end function

   type(base(20,4)) function div(a, b)
      class(base(*,4)), intent(in) :: a, b

      div%i = a%i / b%i

      print *, 'div'

   end function


end module

program genericOperatorPrecedence002
   use m

   type(base(20,4)) :: b1, b2, b3, b4

   b1 = base(20,4)(2) ** base(20,4)(2)
   b2 = base(20,4)(4) / base(20,4)(2)
   print *, b1%i, b2%i

   b3 = b1 ** b2 / b2 ! 16 / 2
   print *, b3%i

   b4 = b3 ** b1 / b3 ** b2    ! ( 8 ** 4 ) / ( 8 ** 2 )
   print *, b4%i

   b3 = base(20,4)(2) ** base(20,4)(2) ** base(20,4)(2) / base(20,4)(2) ** base(20,4)(2) ! ( 2 ** 2 ** 2 ) / ( 2 ** 2 )
   print *, b3%i

   b4 = b1 ** base(20,4)(2) / base(20,4)(4) ** base(20,4)(2) / base(20,4)(1) ** base(20,4)(4)
   print *,  b4%i

end program
