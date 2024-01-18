!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : C460: try generic binding pointing to procedure pointer component
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

   interface
      integer function myintf( a )
         integer, intent(in) :: a
      end function
   end interface

   type base
      integer i
      procedure(myintf), pointer, nopass :: pptr
      contains
         generic :: aa => pptr
   end type


end module

program genericC460GenericName001d
end program
