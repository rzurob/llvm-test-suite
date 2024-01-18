! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/08/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               module used in dtpPass011 program
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m1
use m
    type, extends(base) :: child
        complex(8) cx(n)

        contains

        procedure :: equal => compareChildData
    end type

    contains

    function compareChildData (b1, b2)
        class(child(*)), intent(in) :: b1
        class(base(*)), intent(in) :: b2

        logical :: compareChildData (max(b1%n, b2%n))

        logical, external :: precision_x6

        compareChildData = b1%base%equal (b2)

        select type (b2)
            class is (child(*))
                do i = 1, min(b1%n, b2%n)
                    compareChildData(i) = compareChildData(i) .and. &
                        precision_x6(b1%cx(i), b2%cx(i))
                end do

            class default
                compareChildData = .false.
        end select
    end function
end module

