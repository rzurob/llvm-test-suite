! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg011a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (elemental function
!                               results used as selector in ASSOCIATE construct)
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

        contains

        procedure :: isDefault => isDefaultBaseVal
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: isDefault => isDefaultChildVal
    end type

    contains

    elemental logical function isDefaultBaseVal (b)
        class (base(4)), intent(in) :: b

        isDefaultBaseVal = (b%id == -1)
    end function

    elemental logical function isDefaultChildVal (b)
        class (child(4,1,*)), intent(in) :: b

        isDefaultChildVal = (b%base%isDefault() .and. &
                            b%name == 'default')
    end function
end module

module m1
use m
    type (child(4,1,20)), save :: c1_m(2:3)

    contains

    integer(4) function test1 (b)
        class (base(4)), intent(out) :: b(5:)

        associate (x => b%isDefault())
            if (.not. all (x)) error stop 1_4
            test1 = ubound (x, 1)
        end associate
    end function
end module

program fArg011a1
use m1
    c1_m%id = (/2,3/)
    c1_m%name = (/'c1_m_2', 'c1_m_3'/)

    if (test1(c1_m) /= 2) error stop 3_4

    if ((.not. c1_m(2)%isDefault()) .or. (.not. c1_m(3)%isDefault())) &
        error stop 4_4
end