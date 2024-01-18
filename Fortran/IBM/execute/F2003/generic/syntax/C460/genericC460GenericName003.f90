!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : C460: specific binding exists in parent type
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
         procedure, pass :: mypass
         procedure, nopass :: mynopass => mypass
   end type

   type, extends(base) :: child
      contains
         generic :: donothing => mypass, mynopass
   end type

   contains

      subroutine mypass (a)
         class(base), intent(in) :: a
      end subroutine

end module

program genericC460GenericName003
end program
