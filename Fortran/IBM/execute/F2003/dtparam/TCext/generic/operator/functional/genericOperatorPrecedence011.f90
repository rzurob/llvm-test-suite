! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/functional/genericOperatorPrecedence011.f
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
!*  DESCRIPTION                : Operator: User Defined Operator (Unary and Binary)
!*                                         defined binary should be lower precedence than defined .EQV.
!*                                         where + should have higher precedence than .EQV.
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
         procedure :: myb
         procedure :: mybt
         generic :: operator(.b.) => myb, mybt
         procedure :: add
         generic :: operator(+) => add
         procedure :: eq
         generic :: operator(==) => eq
   end type

   contains

   type(base(4)) function myb (a, b)
      class(base(4)), intent(in) :: a,b
      myb%i = a%i - b%i

      print *, 'myb', myb, '=', a%i, '.b.', b%i

   end function

   logical function mybt (a, b)
      class(base(4)), intent(in) :: a
      logical, intent(in) :: b
      mybt = b

      print *, 'mybt', mybt, '=', b

   end function

   type(base(4)) function add (a, b)
      class(base(4)), intent(in) :: a,b
      add%i = a%i + b%i

      print *, 'add', add, '=', a%i, '+', b%i

   end function

   logical function eq (a, b)
      class(base(4)), intent(in) :: a,b
      eq = ( a%i == b%i )

      print *, 'eq', eq, ':', a%i, '==', b%i

   end function

end module

program genericOperatorPrecedence011
   use m

   type(base(4)) :: b1, b2, b3, b4

   b1 = base(4)(15) .b. base(4)(5)
   b2 = base(4)(10) + base(4)(10)
   b3 = base(4)(50) .b. base(4)(10) + base(4)(5) + base(4)(5)
   b4 = base(4)(20) + base(4)(50) .b. base(4)(10) + base(4)(20)

   print *, b1
   print *, b2
   print *, b3
   print *, b4

   if ( .not. ( b1 + b2 .eq. b3 ) ) error stop 1_4
   if ( .not. ( b1 + b2 == b3 ) )   error stop 2_4
   
   if ( b1 .b. b2 == b3 + b4 )      error stop 3_4
   if ( b1 .b. b2 .eq. b3 + b4 )    error stop 4_4

end program
