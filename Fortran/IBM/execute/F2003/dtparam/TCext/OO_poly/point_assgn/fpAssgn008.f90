! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn008.f
! opt variations: -qnok -ql -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn008.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited poly-pointer
!                               as the structure component; used in pointer
!                               assignment)
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
    end type

    type, extends(base) :: child    ! (4)
        integer(k1) :: id
    end type
end module

module m1
    type container(k2)    ! (4)
        integer, kind :: k2
        class(*), pointer :: data => null()
    end type
end module

program fpAssgn008
use m
use m1
    type(container(4)) :: c, cc

    integer*4, target :: i
    type (base(4)), target :: b1
    type (child(4)), target :: c1

    c%data => i

    if (.not. associated (c%data, i)) error stop 1_4

    nullify (c%data)

    c%data => b1

    if ((.not. associated (c%data)) .or. associated (c%data, b1)) error stop 2_4

    c%data => c1

    if (.not. associated (c%data, c1)) error stop 3_4

    cc = c

    if (.not. associated (cc%data, c1)) error stop 4_4

    c%data => c%data

    if (.not. associated (c%data, c1)) error stop 5_4
end
