!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg030.f
! %VERIFY: fArg030.out:fArg030.vf
! %STDIN:
! %STDOUT: fArg030.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy-arg with
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
    type base
        integer*4 :: id = 1

        contains

        procedure :: print => printBase
        final :: finalizeBaseRank1, finalizeBase
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'

        contains

        procedure :: print => printChild
        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child), intent(in) :: c(:)

        print *, 'finalizeChildRank1'
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b (:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

module m1
use m
    type container
        class (base), allocatable :: data(:)
    end type

    contains

    subroutine printData (co)
        class (container), intent(in) :: co

        if (allocated (co%data)) then
            print *, lbound(co%data, 1), ubound(co%data,1)
            do i = lbound(co%data, 1), ubound(co%data,1)
                call co%data(i)%print
            end do
        end if
    end subroutine
end module

program fArg030
use m1
    class (base), allocatable :: b1(:)

    allocate (b1(2:3), source = (/child(2,'test2'), child(3,'test3')/))

    call printData (container(b1))

    call b1(2)%print
    call b1(3)%print
end
