!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C461: Generic type bound with operator and do not
!*                                     specify pass object dummy argument (unary operator)
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
      integer :: i = -1
   contains
      procedure, nopass, private :: not
      generic :: operator(.not.) => not
   end type

   type, extends(base) :: child
      character(3) :: c = 'xxx'
      contains
         procedure, nopass :: childnot
         generic :: operator(.not.) => childnot
   end type

   contains

      type(base) function not ( a )
         class(base), intent(in) :: a

         not%i = -1 * a%i

      end function

      type(child) function childnot ( a )
         type(child), intent(in) :: a

         childnot%base = .not. a%base
         childnot%c = 'xxx'

      end function

end module


program genericC461Operator002d
end program
