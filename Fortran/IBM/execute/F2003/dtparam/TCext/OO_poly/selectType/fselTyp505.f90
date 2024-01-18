! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/fselTyp505.f
! opt variations: -qnok -qnol

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fselTyp505.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (nested select type and intrinsic
!                               assignment interacted)
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

program fselTyp505
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data
    end type

    type (base(4,20)) :: b1, b2

    allocate (b1%data, source=100)

    b2 = b1

    if (.not. allocated (b2%data)) error stop 1_4

    select type (x => b2%data)
        type is (integer)
            if (x /= 100) error stop 2_4

            x = 2 * x

            b1 = b2

            select type (y => b1%data)
                type is (integer)
                    if (y /= 200) error stop 3_4
            end select
    end select
    end
