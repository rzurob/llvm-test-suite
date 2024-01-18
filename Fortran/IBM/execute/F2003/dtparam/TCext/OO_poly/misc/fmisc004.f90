! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/misc/fmisc004.f
! opt variations: -ql

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 283226)
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
    type A(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type
end module

program data1
    call abc
end

subroutine abc
use m
    type (A(4)) :: a1, a2

    data a1, a2%i /A(4)(10), 10/

    if ((a1%i /= 10) .or. (a2%i /= 10)) error stop 1_4
end

