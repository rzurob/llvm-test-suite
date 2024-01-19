!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C459:  using private-binding-stmt
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
         private
         generic :: operator(.myadd.) => add_i
         procedure, pass ( passobj ) :: add_i
         generic, public :: operator(.myadd.) => add_base
         procedure, pass, public :: add_base
   end type

   contains

      function add_i ( int, passobj )
         class(base), intent(in) :: passobj
         integer, intent(in) :: int
         type(base) :: add_i
         add_i%i = passobj%i + int
      end function

      function add_base ( passobj, base )
         class(base), intent(in) :: passobj, base
         type(base) :: add_base
         add_base%i = passobj%i + base%i
      end function

end module

program genericC459Operator004d
end program
