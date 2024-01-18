! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd508a.f
! opt variations: -qck -qnol -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (inherited binding and
!*                               overridden binding for two extended types)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: value

        contains

        procedure :: compare => aGtB
    end type

    contains

    !! for the base type, returns true only if the caller has a larger value
    logical function aGtB (a, b)
        class (base(*,4)), intent(in) :: a, b

        aGtB = (a%value > b%value)
    end function

end module

module m1
use m
    type, extends (base) :: child1    ! (20,4)
        character(n1) :: name
    end type

    type, extends (base) :: child2    ! (20,4)
        logical(k1) :: flag

        contains

        procedure :: compare => aLeb
    end type

    contains

    !! for child2 type, returns true only the caller has a smaller/equal value
    logical function aLeb (a, b)
        class (child2(*,4)), intent(in) :: a
        class (base(*,4)), intent(in) :: b

        aLeb = (a%value <= b%value)
    end function
end module

program ftpbnd508a
use m1
    type (child1(20,4)) :: c1

    type (child2(20,4)) :: c2

    c1 = child1(20,4) (10, 'c1')

    c2 = child2(20,4) (20, (1==1))

    if (c1%compare(c2)) error stop 1_4

    if (c2%compare(c1)) error stop 2_4
end
