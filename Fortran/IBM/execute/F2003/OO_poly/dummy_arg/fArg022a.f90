!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg022a.f
! %VERIFY: fArg022a.out:fArg022a.vf
! %STDIN:
! %STDOUT: fArg022a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-actual-arg
!                               associated with nonpoly-dummy-arg with
!                               INTENT(OUT) attribute)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
        procedure, non_overridable :: assgnId => assignID2Base
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent (in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine assignID2Base (b, id)
        class (base), intent(inout) :: b

        b%id = id
    end subroutine
end module

program fArg022a
use m
    interface assignment(=)
        subroutine base2Base (b1, b2)
        use m
            type (base), intent(out) :: b1
            class (base), intent(in) :: b2
        end subroutine
    end interface

    class (base), allocatable :: b1(:)

    type (child) :: c1 = child (100, 'c1_static')

    allocate (b1(3), source=(/child(1, 'b1_1'), child(2, 'b1_2'), &
                child(3,'b1_3')/))


    !
    b1(1) = b1(3)

    b1(2) = c1

    call b1(1)%print
    call b1(2)%print
    call b1(3)%print
end

subroutine base2Base (b1, b2)
use m
    type (base), intent(out) :: b1
    class (base), intent(in) :: b2

    call b1%assgnID(b2%id)
end subroutine
