!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C461: Generic type bound with operator and do not
!*                                     specify pass object dummy argument (binary operator)
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
      procedure, nopass, private :: add
      generic :: operator(+) => add, addwithint
      procedure, nopass :: addwithint
   end type

   contains

      type(base) function add ( a, b )
         class(base), intent(in) :: a
         class(base), intent(in) :: b

         add%i = a%i + b%i

      end function

      type(base) function addwithint ( a, b )
         class(base), intent(in) :: a
         integer , intent(in) :: b

         addwithint%i = a%i + b

      end function

end module


program genericC461Operator001d
end program
