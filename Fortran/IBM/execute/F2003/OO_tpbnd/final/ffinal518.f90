! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization would not happen for
!*                               structure constructor used in data statement)
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

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal518
    call abc
    call abc
end

subroutine abc
use m
    type (base) :: b1, b2

    data b2, b1%id /base(10), 10/

    logical, save :: firstTime = .true.

    if (firstTime) then
        firstTime = .false.
    else
        b1%id = b1%id + 1
    end if

    print *, b1
end subroutine
