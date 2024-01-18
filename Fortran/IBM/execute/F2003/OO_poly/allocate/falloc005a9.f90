!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005a9.f
! %VERIFY: falloc005a9.out:falloc005a9.vf
! %STDIN:
! %STDOUT: falloc005a9.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (array constructor in source-expr)
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

program falloc005a9
use m
    class (base), pointer :: b1
    class (base), allocatable :: b2(:), b3(:)

    allocate (b1, source=child (id = 10, name = 'b1'))

    allocate (b2(0:1), source=(/child(name='b1_1'), child(id=20)/))

    allocate (b3(3), source=(/b1, b2/))

    call b2(0)%print
    call b2(1)%print

    call b3(1)%print
    call b3(2)%print
    call b3(3)%print

    deallocate (b1, b2, b3)
end
