!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc003a1.f
! %VERIFY: falloc003a1.out:falloc003a1.vf
! %STDIN:
! %STDOUT: falloc003a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (source=; test the reshape used as the
!                               source-expr for rank-two array)
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
        integer(4) :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name = 'default'
        type (base) :: data (3)

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name

        do i = 1, 3
            call b%data(i)%print
        end do
    end subroutine
end module

program falloc003a1
use m
    class (base), allocatable :: b1 (:,:)

    type (child) :: c1(4)

    allocate (b1(2,2), source=reshape(c1, (/2,2/)))

    call b1(1,1)%print
    call b1(1,2)%print
    call b1(2,1)%print
    call b1(2,2)%print
end
