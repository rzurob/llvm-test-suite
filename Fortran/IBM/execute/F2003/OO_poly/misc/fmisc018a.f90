! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 289663; last TC)
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
        integer i
    end type

    integer base

    contains

    type (base) function f1(ff1)
        integer(8), parameter :: base = 8

        interface
            type(base) function ff1(ii)
                import base
                integer(base) :: ii
            end function
        end interface

        f1 = ff1(10_8)
    end function
end module


type (base) function ff1 (ii)
use m
    integer(8) :: ii

    ff1%i = ii *2
end function


program fmisc018a
use m
    interface
        type(base) function ff1(ii)
        use m, only:base
            integer(8) :: ii
        end function
    end interface

    type (base) b1

    b1 = f1(ff1)

    if (b1%i /= 20) error stop 1_4
end
