! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of the temps created by
!*                               structure constructor in a DO construct, in
!*                               particular DO WHILE loop)
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
        integer*4 :: id

        contains

        final :: finalizeBase
    end type

    type (base) :: b1_m(3)

    contains

    subroutine finalizeBase (b)
        type (base) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal515a3
use m
    interface operator (>)
        logical function b1GTb2 (b1, b2)
        use m
            type (base), intent(in) :: b1, b2
        end function
    end interface

    b1_m%id = 1

    i = 1

    do while ((i <= 3) .and. (b1_m(i) > base(i-2)))
        print *, i
        i = i + 1
    end do

    i = 1

    print *, 'send loop'

    do while ((i <= 3) .and. (b1_m(i) > base (0)))
        print *, i
        b1_m(i) = base(i)

        i = i + 1
    end do

    print *, 'end'
end

logical function b1GTb2 (b1, b2)
use m
    type (base), intent(in) :: b1, b2

    b1GTb2 = (b1%id > b2%id)
end function
