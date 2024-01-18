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
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Operator: named-constant (parameter) should still invoke the generic tb procedures
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
      character(3) :: c = 'xxx'
      contains
         procedure :: ab
         generic :: operator(//) => ab
   end type


   type, extends( base ) :: child
      contains
         procedure :: ab => c
         generic :: operator(//) => ab !<- specifying the same binding name twice
   end type

   contains

   character(6) function ab ( a, b )
      class(base), intent(in) :: a, b
      ab = a%c // b%c
   end function

   character(6) function c ( a, b )
      class(base), intent(in) ::  b
      class(child), intent(in) ::  a
      c = a%c(1:1) // b%c(1:1) // a%c(2:2) // b%c(2:2) // a%c(3:3) // b%c(3:3)
   end function


end module

program genericOperatorParameter001
   use m

   type(base), parameter  :: b1 = base('IBM')
   type(base), parameter  :: b2 = base('FTN')
   type(child), parameter :: c1 = child('xxx')

   print *, b1 // b2

   print *, c1 // b1

   print *, b2 // c1

   print *, c1 // b2

end program
