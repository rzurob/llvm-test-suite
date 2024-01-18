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
!*  DESCRIPTION                : Operator: 12.4.5 Resolving type-bound procedure references
!*                                         i ) contains both scalar and elemental references for unary op with interface
!*
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
         procedure :: neg
         generic :: operator (-) => neg
   end type

   contains

   type(base) function neg (a)
      class(base), intent(in) :: a

      neg%i = -1 * a%i
      print *, "neg"
   end function

end module

program genericOperatorResolve001d2
   use m

   interface operator (-)
      type(base) elemental function elementalneg (a)
         import base
         class(base), intent(in) :: a
      end function
   end interface

end program


type(base) elemental function elementalneg (a)
   use m, only: base
   class(base), intent(in) :: a

   elementalneg%i = -1 * a%i

end function