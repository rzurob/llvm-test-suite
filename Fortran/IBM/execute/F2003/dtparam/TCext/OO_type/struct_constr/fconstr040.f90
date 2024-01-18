! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr040.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr040.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (sequence component in
!*                               the structure constructor)
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
    type seq1(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        sequence
        integer(k1)   :: x = 1
        integer(k2)   :: y = 1
    end type
end module

module m1
use m
    type base(k3)    ! (4)
        integer, kind     :: k3
        type(seq1(k3,k3)) :: s1
    end type

    type (seq1(4,4)), save :: s1_m = seq1(4,4)()
end module

program fconstr040

use m1
    interface
        subroutine updateBase (b)
        use m1, only : base
            type (base(4)), intent(inout) :: b
        end subroutine
    end interface

    type (base(4)) :: b1

    b1 = base(4) (s1 = s1_m)

    if ((b1%s1%x /= 1) .or. (b1%s1%y /= 1)) error stop 1_4

    call updateBase (b1)

    if ((b1%s1%x /= 1) .or. (b1%s1%y /= 10)) error stop 2_4
end

subroutine updateBase (b)
use m1, only : base
    type (base(4)), intent(inout) :: b

    type seq1(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        sequence
        integer(k1)   :: x = 10
        integer(k2)   :: y = 10
    end type

    type (seq1(4,4)) :: s1

    if ((s1%x /= 10) .or. (s1%y /= 10)) error stop 10_4

    s1 = seq1(4,4)(y= 10, x = 1)

    b = base(4)(s1 = s1)
end subroutine
