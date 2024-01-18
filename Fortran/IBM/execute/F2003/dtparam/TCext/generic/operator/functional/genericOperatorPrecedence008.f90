! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/functional/genericOperatorPrecedence008.f
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
!*  DESCRIPTION                : Operator: Binary Operators (numeric and logical operators)
!*                                         Try + and .not.
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
      integer(k1)   :: i
      contains
         procedure :: add
         procedure :: not
         generic :: operator(+) => add
         generic :: operator(.not.) => not
   end type

   contains

   function add (a, b)
      type(base(20,4)) :: add
      class(base(*,4)), intent(in) :: a, b

      add%i = a%i + b%i

      print *, 'add'
   end function

   function not (a)
      type(base(20,4)) :: not
      class(base(*,4)), intent(in) :: a

      not%i = -1*a%i
      print *, 'not'

   end function

end module

program genericOperatorPrecedence008
   use m

   type(base(20,4)) :: b1, b2, b3, b4

   b1 = .not. base(20,4)(-1)
   print *, b1%i

   b2 = b1 + b1
   print *, b2%i

   b3 = .not. b2 + b1
   print *, b3%i

   b4 = .not. b1 + (.not. b2) + b3
   print *, b4%i

end program
