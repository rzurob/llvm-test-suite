!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005a23.f
! %VERIFY: falloc005a23.out:falloc005a23.vf
! %STDIN:
! %STDOUT: falloc005a23.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (transpose() used in source-expr)
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
        integer(4) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15) :: name

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
    end subroutine
end module

program falloc005a23
use m
    class (base), allocatable :: b1(:,:), b2(:,:), b3(:,:)
    type (child) :: c1(2:3,3)

    c1%id = reshape ((/1,2,3,4,5,6/), (/2,3/))
    c1%name = reshape ((/'c1_1','c1_2','c1_3','c1_4','c1_5','c1_6'/), (/2,3/))

    allocate (b1(2, 3), source=c1)
    allocate (b2(3,2), source=transpose(c1))
    allocate (b3(3,2), source=transpose(b1))

    do i = 1, 3
        do j = 1, 2
            call b2(i, j)%print
            call b3(i, j)%print
        end do
    end do
end
