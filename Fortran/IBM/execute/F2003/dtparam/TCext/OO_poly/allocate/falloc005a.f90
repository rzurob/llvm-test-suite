! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (named constants used as the
!                               source-expr)
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
end module

program falloc005a
use m
    type (base(4)), parameter :: b_const = base(4)(100)
    complex(4), parameter :: cx_const = (1.0d0, 0.0d0)

    class (base(4)), allocatable :: b1(:)
    complex(4), pointer :: cx1
    logical(4) precision_x8

    allocate (b1(2), source=b_const)

    allocate (cx1, source=cx_const)

    if (any (b1%id /= 100)) error stop 1_4

    if (.not. precision_x8 (cx1, cx_const)) error stop 2_4

    deallocate (b1, cx1)
end
