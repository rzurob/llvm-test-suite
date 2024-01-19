! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/functional/genericOperatorInit001d.f
! opt variations: -qnol

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

  type base(n1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: n1
     integer(k1)   :: i = -999
     contains
        procedure :: add
        generic :: operator(+) => add
  end type

  contains

  type(base(20,4)) function add (a, b)
     class(base(*,4)), intent(in) :: a, b

     add%i = a%i + b%i

  end function

end module

  use m

  integer :: i = ( 5 - 4 )
  type(base(20,4)) :: b1 = ( base(20,4)(1) + base(20,4)(2) )

end
