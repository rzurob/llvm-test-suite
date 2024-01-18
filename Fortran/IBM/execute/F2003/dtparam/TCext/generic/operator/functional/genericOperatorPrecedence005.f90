! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/functional/genericOperatorPrecedence005.f
! opt variations: -ql

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
!*                                         defined unary + should be higher precedence than defined binary +
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
         procedure :: add
         procedure :: positive
         generic :: operator(+) => positive
         generic :: operator(+) => add
   end type

   contains


   type(base(4)) function positive(a)
      class(base(4)), intent(in) :: a

      if ( a%i < 0 ) then
         positive%i = -1*a%i
      else
         positive%i = a%i
      end if

      print *, positive%i, ' is positive of ', a%i

   end function
   
   type(base(4)) function add(a,b)
      class(base(4)), intent(in) :: a,b

      add%i = a%i + b%i

      print *, add%i, ' = ', a%i,' add ', b%i

   end function


end module

program genericOperatorPrecedence005
   use m

   type(base(4)) :: b1, b2, b3, b4

   b1 = base(4)(2) + base(4)(2)
   b2 = +base(4)(-3)
   print *, b1%i, b2%i

   b3 = +b1 + b2 + b2
   print *, b3%i

   b4 = b3 + b1 + (+b3 + (+b2) )
   print *, b4%i

   b3 = base(4)(2) + (+base(4)(-4)) + base(4)(-6) + (+base(4)(-8)) + (+base(4)(-10))
   print *, b3%i

   b4 = b1 + (+base(4)(-6)) + (+base(4)(8)) + base(4)(10) + (+base(4)(-12)) + base(4)(14)
   print *,  b4%i

end program
