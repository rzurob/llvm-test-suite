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

  type(base) :: b1 = base(10) + 1

  contains

  type(base) function add (a, b)
     class(base), intent(in) :: a
     integer, intent(in) :: b

     add%i = a%i + b

  end function

end module


end
