! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/dummy_arg/fArg508a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) dummy-arg
!                               requires default initialization upon invocation)
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

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'
    end type

    class (child(4,1,20)), allocatable :: c1_m(:)

    contains

    logical function isDefault (b)
        class (base(4)), intent(out) :: b

        isDefault = (b%id == -1)
    end function
end module

program fArg508a
use m
    allocate (c1_m(3))

    c1_m%id = (/1,2,3/)
    c1_m%name = (/'c1_m_1', 'c1_m_2', 'c1_m_3'/)

    if (.not. isDefault (c1_m(3)%base)) error stop 1_4

    if (.not. isDefault (c1_m(2))) error stop 2_4

    print *, c1_m(1)%id, c1_m(1)%name
    print *, c1_m(2)%id, c1_m(2)%name
    print *, c1_m(3)%id, c1_m(3)%name
end
