!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fselTyp505a.f
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
!*  DESCRIPTION                : select type (work around of the nested select
!                               type problem in fselTyp505.f)
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

program fselTyp505a
    type base
        class(*), allocatable :: data
    end type

    logical precision_r8
    type (base) :: b1, b2

    allocate (b1%data, source=100)

    b2 = b1

    if (.not. allocated (b2%data)) error stop 1_4

    select type (x => b2%data)
        type is (integer)
            if (x /= 100) error stop 2_4

            deallocate (b1%data)
            allocate (b1%data, source=1.0_8)
    end select

    b2 = b1

    select type (x => b2%data)
        type is (real(8))
            if (.not. precision_r8(x, 1.0_8)) error stop 3_4
    end select
end
