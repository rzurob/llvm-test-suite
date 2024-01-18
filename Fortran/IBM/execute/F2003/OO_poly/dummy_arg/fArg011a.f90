! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : arguement association (binding call for
!                               associate-name in ASSOCIATE construct)
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
    type base
        integer(4) :: id = -1

        contains

        procedure :: isDefault => isDefaultBaseVal
    end type

    type, extends(base) :: child
        character(20) :: name = 'default'

        contains

        procedure :: isDefault => isDefaultChildVal
    end type

    contains

    logical function isDefaultBaseVal (b)
        class (base), intent(in) :: b

        print *, 1

        isDefaultBaseVal = (b%id == -1)
    end function

    logical function isDefaultChildVal (b)
        class (child), intent(in) :: b

        print *, 2

        isDefaultChildVal = (b%base%isDefault() .and. &
                            b%name == 'default')
    end function
end module

module m1
use m
    type (child), save :: c1_m(2:3)

    contains

    integer(4) function test1 (b)
        class (base), intent(out) :: b(5:)

        associate (x => b)
            if (lbound(x,1) /= 5) error stop 1_4

            do i = 5, ubound(x,1)
                if (.not. x(i)%isDefault()) error stop 2_4
            end do

            test1 = ubound(x,1)
        end associate
    end function
end module

program fArg011a
use m1
    c1_m%id = (/2,3/)
    c1_m%name = (/'c1_m_2', 'c1_m_3'/)

    if (test1(c1_m) /= 6) error stop 3_4

    if ((.not. c1_m(2)%isDefault()) .or. (.not. c1_m(3)%isDefault())) &
        error stop 4_4
end
