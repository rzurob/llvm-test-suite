! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/functional/genericOperatorPrecedence006.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Unary Operators and Binary Operators
!*                                         defined unary +,- should be higher precedence than defined binary +,-
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
         procedure :: positive
         procedure :: sub
         procedure :: negative
         generic :: operator(+) => positive
         generic :: operator(+) => add
         generic :: operator(-) => negative, sub
   end type

   contains

   type(base(20,4)) function negative(a)
      class(base(*,4)), intent(in) :: a

      if ( a%i > 0 ) then
         negative%i = -1*a%i
      else
         negative%i = a%i
      end if

      print *, negative%i, ' is negative of ', a%i

   end function

   type(base(20,4)) function sub(a,b)
      class(base(*,4)), intent(in) :: a,b

      sub%i = a%i - b%i

      print *, sub%i, ' = ', a%i,' sub ', b%i

   end function

   type(base(20,4)) function positive(a)
      class(base(*,4)), intent(in) :: a

      if ( a%i < 0 ) then
         positive%i = -1*a%i
      else
         positive%i = a%i
      end if

      print *, positive%i, ' is positive of ', a%i

   end function

   type(base(20,4)) function add(a,b)
      class(base(*,4)), intent(in) :: a,b

      add%i = a%i + b%i

      print *, add%i, ' = ', a%i,' add ', b%i

   end function


end module

program genericOperatorPrecedence006
   use m

   type(base(20,4)) :: b1, b2, b3, b4

   b1 = base(20,4)(2) + base(20,4)(2)
   b2 = +base(20,4)(-3)

   print *, b1%i, b2%i

   b3 = base(20,4)(4) - base(20,4)(2)
   b4 = -base(20,4)(+5)

   print *, b3%i, b4%i

   b1 = b1 + (-b2) - (+b3) + (+b4)
   print *, b1%i

   b2 = - b1 - (-b2) - b3 - b4
   print *, b2%i

   b3 = -b1 + b2 - b3 + (-b4)
   print *, b3%i

end program
