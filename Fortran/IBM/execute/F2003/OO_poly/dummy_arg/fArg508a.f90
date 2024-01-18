!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg508a.f
! %VERIFY: fArg508a.out:fArg508a.vf
! %STDIN:
! %STDOUT: fArg508a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) dummy-arg
!                               requires default initialization upon invocation)
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
        integer*4 :: id = -1
    end type

    type, extends(base) :: child
        character*20 :: name = 'default'
    end type

    class (child), allocatable :: c1_m(:)

    contains

    logical function isDefault (b)
        class (base), intent(out) :: b

        isDefault = (b%id == -1)
    end function
end module

program fArg508a
use m
    allocate (c1_m(3))

    c1_m%id = (/1,2,3/)
    c1_m%name = (/'c1_m_1', 'c1_m_2', 'c1_m_3'/)

    if (.not. isDefault (c1_m(3)%base)) error stop 1_4

    if (.not. isDefault (c1_m(2))) error stop 2_4

    print *, c1_m(1)%id, c1_m(1)%name
    print *, c1_m(2)%id, c1_m(2)%name
    print *, c1_m(3)%id, c1_m(3)%name
end
