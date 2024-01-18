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
!*  DESCRIPTION                : C459: base type has public generic tb, and child type has private
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
         generic, public :: operator(*) => times_i
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
         generic, private :: operator(*) => timeschild_i
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

program genericC459Operator005d
end program
