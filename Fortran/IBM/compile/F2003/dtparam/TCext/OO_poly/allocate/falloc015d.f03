! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc015d.f
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    class(*), allocatable :: x(:), x1
    class (base(4,20)), allocatable :: b1

    allocate (integer(4) :: x1)
    print *, kind (x), kind(x1)
    print *, kind (b1)
end