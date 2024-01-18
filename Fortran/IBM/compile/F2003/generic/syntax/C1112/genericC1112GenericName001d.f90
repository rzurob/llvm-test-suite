
!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : C1112: generic-spec in use only statement shall not specify
!*                                      a generic binding
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
      integer :: i
      contains
         procedure, pass :: add
         procedure, pass :: addint
         generic :: operator(+) => add, addint
   end type

   type base1
      integer :: i
      contains
         procedure, pass :: minus
         procedure, pass :: subtract
         generic :: operator(+) => minus, subtract
   end type

   contains

      type(base) function add(a,b)
         class(base), intent(in) :: a
         class(base), intent(in) :: b

         add = base(a%i+b%i)

      end function

      type(base) function addint(a,b)
         class(base), intent(in) :: a
         integer, intent(in) :: b

         addint= base(a%i+b)

      end function

      type(base1) function minus(a)
         class(base1), intent(in) :: a

         minus = base1(-1*a%i)

      end function

      type(base1) function subtract(a,b)
         class(base1), intent(in) :: a
         integer, intent(in) :: b

         subtract= base1(a%i+b)

      end function

end module

program genericC1112GenericName001d
   use m, only: operator(+)
   use m, only: operator(-)
end program
