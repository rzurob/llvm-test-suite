!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: 12.4.5 Resolving type-bound procedure references
!*                                         ii ) contains both scalar and elemental references for binary op
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
         procedure :: elementaladd
         generic :: operator (+) => add, elementaladd
   end type

   contains

   type(base) function add (a,b)
      class(base), intent(in) :: a, b

      add%i = a%i + b%i

   end function

   type(base) elemental function elementaladd (a,b)
      class(base), intent(in) :: a, b

      elementaladd%i = a%i + b%i

   end function

end module

program genericOperatorResolve002d
end program
