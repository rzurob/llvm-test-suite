!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguity of interfaces with the same generic operators with different from
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


module type

   type base
      integer :: i
      contains
         procedure :: myequalmod
         generic :: operator(==) => myequalmod
   end type

   interface operator(.eq.)
      logical function myequal( a, b )
         import base
         class(base), intent(in) :: a, b
      end function
   end interface

   contains

      logical function myequalmod( a, b )
         class(base), intent(in) :: a, b

         myequalmod = ( a%i == b%i )
      end function

end module

program genericAmbiguityInterface013d
end program
