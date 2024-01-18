! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/misc/fmisc018a.f
! opt variations: -ql

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc018a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    integer base

    contains

    type (base(4)) function f1(ff1)
        integer(8), parameter :: base = 8

        interface
            type(base(4)) function ff1(ii)
                import base
                integer(base) :: ii
            end function
        end interface

        f1 = ff1(10_8)
    end function
end module


type (base(4)) function ff1 (ii)
use m
    integer(8) :: ii

    ff1%i = ii *2
end function


program fmisc018a
use m
    interface
        type(base(4)) function ff1(ii)
        use m, only:base
            integer(8) :: ii
        end function
    end interface

    type (base(4)) b1

    b1 = f1(ff1)

    if (b1%i /= 20) error stop 1_4
end
