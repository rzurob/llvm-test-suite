!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C459: define generic TB with same generic name with different access-spec
!*                                     in extended types, make sure public accessibility is accessible outside module
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

module n
   use m, only: base

   type, extends(base) :: child
      real :: r = 0.0
      contains
         generic :: operator(*) => timeschild_i
         procedure, pass, private :: timeschild_i => childtimesi
   end type

   contains

      function childtimesi ( passobj, int )
         class(child), intent(in) :: passobj
         real, intent(in) :: int
         type(child) :: childtimesi
         childtimesi%i = passobj%i * int
         childtimesi%r = passobj%r * int

      end function

end module

program genericC459Operator002
end program
