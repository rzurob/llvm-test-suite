!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : misc: defined-operator being some illegal operator
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
         generic :: operator(?) => a
         procedure, pass :: a
   end type

   type base1
      integer i
      contains
         generic :: operator(%) => b
         procedure, pass :: b
   end type

   contains

      type(base) function a ( obj, passobj )
         class(base), intent(in) :: obj
         class(base), intent(in) :: passobj

         a%i = obj%i ** passobj%i

      end function

      type(base1) function b ( obj, passobj )
         class(base1), intent(in) :: obj
         class(base1), intent(in) :: passobj

         b%i = obj%i ** passobj%i

      end function

end module

program genericR452Operator002d
end program
