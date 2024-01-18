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
!*  DESCRIPTION                : Operator: User Defined Operator (Unary and Binary)
!*                                         defined unary should be higher precedence than defined binary
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

   type base
      integer i
      contains
         procedure :: myu
         generic :: operator(.unary.) => myu
         procedure :: myb
         generic :: operator(.binary.) => myb
   end type

   contains

   type(base) function myu ( a )
      class(base), intent(in) :: a

      myu = a
      myu%i = myu%i * (-1)

      print *, 'myu'

   end function

   type(base) function myb ( a, b )
      class(base), intent(in) :: a, b

      myb%i = a%i + b%i

      print *, 'myb'

   end function

end module

program genericOperatorPrecedence009
   use m

   type(base) :: b1, b2, b3, b4

   b1 = .unary. base(-10)
   b2 = b1 .binary. base(10)

   print *, b1%i, b2%i

   b3 = .unary. b1 .binary. b2 .binary. base(20)

   print *, b3%i

   b4 = .unary. b3 .binary. b2 .binary. (.unary. b1 )

   print *, b4%i

end program
