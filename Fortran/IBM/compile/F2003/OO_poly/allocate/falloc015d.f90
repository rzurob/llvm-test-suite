! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/31/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (kind() only accepts the intrinsic
!                               types as the actual-arg)
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

program falloc015d
    type base
    end type

    class(*), allocatable :: x(:), x1
    class (base), allocatable :: b1

    allocate (integer(4) :: x1)
    print *, kind (x), kind(x1)
    print *, kind (b1)
end
