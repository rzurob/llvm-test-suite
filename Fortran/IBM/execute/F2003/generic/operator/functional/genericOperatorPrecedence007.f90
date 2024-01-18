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
!*  DESCRIPTION                : Operator: Binary Operators (numeric and character operators)
!*                                         Try // and +
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
      character(4) :: c
      contains
         procedure :: add
         procedure :: con
         generic :: operator(+) => add
         generic :: operator(//) => con
  end type

   contains

   function add (a, b)
      type(base) :: add
      class(base), intent(in) :: a, b

      add%c(1:2) = a%c(1:2)
      add%c(3:4) = b%c(1:2)

      print *, 'add'
   end function

   function con (a, b)
      type(base) :: con
      class(base), intent(in) :: a, b

      con%c(1:2) = a%c(1:2)
      con%c(3:4) = b%c(3:4)

      print *, 'concat'

   end function

end module

program genericOperatorPrecedence007
   use m

   type(base) b1, b2

   b1 = base ('abcd')
   b2 = base ('ABCD')

   b1 = b1 + b2
   print *, b1%c

   b2 = b1 // b2
   print *, b2%c

   b1 = b1 // b2 + b1
   print *, b1%c

   b2 = b2 // b2 // b2 + b2
   print *, b2%c

end program
