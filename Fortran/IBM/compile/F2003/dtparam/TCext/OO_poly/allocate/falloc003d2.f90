! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc003d2.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
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

program falloc003d2
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(base) :: child(k2)    ! (4,20,4)
        integer, kind :: k2
        integer(k2)      id
    end type

    type (base(4,20)), pointer :: b1

    class (child(4,20,4)), allocatable :: c1

    allocate (b1, source=child(4,20,4)(10))   !<-- illegal

    allocate (c1, source=base(4,20)())      !<-- illegal
end
