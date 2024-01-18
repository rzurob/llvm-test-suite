! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/fselTyp506a1.f
! opt variations: -qnok -ql

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fselTyp506a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (used for verifying results for
!                               defined assignment for types with unlimited
!                               poly-allocatable component)
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
        class(*), allocatable :: data
    end type

    interface assignment(=)
        subroutine assgnB1B2 (b1, b2)
        import base
            type (base(4)), intent(inout) :: b1
            type (base(4)), intent(in) :: b2
        end subroutine
    end interface
end module

program fselTyp506a1
use m
    type (base(4)) :: b1, b2

    allocate (b2%data, source=(1.2e1, .5e0))

    b1 = b2

    select type (x => b1%data)
        type is (integer)
            if (x /= 12) error stop 1_4
        class default
            error stop 2_4
    end select
end


subroutine assgnB1B2 (b1, b2)
use m, only : base
    type (base(4)), intent(out) :: b1
    type (base(4)), intent(in) :: b2

    if (.not. allocated (b2%data)) return

    select type (x => b2%data)
        type is (real)
            allocate (b1%data, source=int(x))

        type is (complex)
            allocate (b1%data, source=int(x))

        class default
            b1 = b2     !<-- use intrinsic assignment
    end select
end subroutine
