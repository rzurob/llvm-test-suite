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
!*                                         ii ) contains both scalar and elemental references for binary op in interface
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
         procedure :: add
         generic :: operator (+) => add
   end type

   contains

   type(base) function add (a,b)
      class(base), intent(in) :: a, b

      add%i = a%i + b%i

   end function


end module

type(base) elemental function elementaladd (a,b)
   use m, only: base
   class(base), intent(in) :: a, b

   elementaladd%i = a%i + b%i

end function

module n
   use m

   interface operator(+)
      type(base) elemental function elementaladd (a,b)
         import base
         class(base), intent(in) :: a, b
      end function
   end interface

end module

program genericOperatorResolve002d2
end program
