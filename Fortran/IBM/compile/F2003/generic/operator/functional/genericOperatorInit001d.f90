!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Diagnostic: initialization expression containing user
!*                                         defined operator
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
     integer :: i = -999
     contains
        procedure :: add
        generic :: operator(+) => add
  end type

  contains

  type(base) function add (a, b)
     class(base), intent(in) :: a, b

     add%i = a%i + b%i

  end function

end module

  use m

  integer :: i = ( 5 - 4 )
  type(base) :: b1 = ( base(1) + base(2) )

end
