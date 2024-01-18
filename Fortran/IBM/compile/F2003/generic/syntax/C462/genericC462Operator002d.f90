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
!*  DESCRIPTION                : C462:
!*                               Section 12.3.2.1.1 - Defined Operations
!(                                  non intent(in) dummy arguments
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

   type :: base
      integer i
      contains
         procedure, pass :: mypower
         generic :: operator(**) => mypower
   end type

   type :: base2
      character(3) :: c
      contains
         procedure, pass :: concat
         generic :: operator(//) => concat

         procedure, pass :: not
         generic :: operator(.not.) => not

   end type

   contains

   function mypower ( a, b )
      class(base), intent(inout) :: a
      class(base), intent(out) :: b
      type(base) :: mypower

      mypower%i = a%i ** b%i
   end function

   function concat ( a, b )
      class(base2), intent(inout) :: a
      class(base2), intent(in)    :: b
      type(base2) :: concat

      concat%c = 'ab'//'C'
   end function

   function not ( a )
      class(base2), intent(out) :: a
      type(base2) :: not

      not%c = 'xxx'

   end function

end module

end
