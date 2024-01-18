!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C459: make sure private is not accessible outside module
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
      integer :: i = 10
      contains
         generic, private :: operator(*) => times_i
         procedure, pass  :: times_i => basetimesi
   end type

   contains

      function basetimesi ( passobj, int)
         class(base), intent(in) :: passobj
         integer, intent(in) :: int
         type(base) :: basetimesi
         basetimesi%i = passobj%i * int
      end function

end module

program genericC459Operator002d
   use m

   type(base) :: b1
   b1 = b1 * 10

end program
