! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/fselTyp506.f
! opt variations: -qnok -ql

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fselTyp506.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (an actual test case belongs to
!                               CLASS bucket; use elemental subroutine for
!                               defined assignment; apply on arrays; use select
!                               type to verify)
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
        elemental subroutine assgnB1B2 (b1, b2)
        import base
            type (base(4)), intent(inout) :: b1
            type (base(4)), intent(in) :: b2
        end subroutine
    end interface
end module

program fselTyp506
use m
    type (base(4)), allocatable :: b3(:), b4(:)

    allocate (b3(0:2), b4(3))

    allocate (b3(0)%data, source=100.8e0)
    allocate (b3(1)%data, source=(1.0e0, .5e1))
    allocate (b3(2)%data, source=-10)

    b4 = b3

    !! verify b4
    select type (x => b4(1)%data)
        type is (integer)
            if (x /= 100) error stop 1_4
        class default
            error stop 2_4
    end select

    select type (x => b4(2)%data)
        type is (integer)
            if (x /= 1) error stop 3_4
        class default
            error stop 4_4
    end select


    select type (x => b4(3)%data)
        type is (integer)
            if (x /= -10) error stop 5_4
        class default
            error stop 6_4
    end select
end


elemental subroutine assgnB1B2 (b1, b2)
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
            b1 = b2
    end select
end subroutine
