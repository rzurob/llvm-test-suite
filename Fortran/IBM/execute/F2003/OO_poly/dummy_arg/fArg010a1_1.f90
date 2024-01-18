!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg010a1_1.f
! %VERIFY: fArg010a1_1.out:fArg010a1_1.vf
! %STDIN:
! %STDOUT: fArg010a1_1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute for
!                               dummy-arg with pointer component; reallocate
!                               pointer component without affecting the original
!                               pointer)
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

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    type container
        class (base), pointer :: data => null()
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

    subroutine allocateTemp (c)
        type (container), value :: c

        print *, 'entering allocateTemp'

        if (associated (c%data))   call c%data%print

        allocate (child:: c%data)

        call c%data%print

        print *, 'leaving allocateTemp'
    end subroutine
end module

program fArg010a1_1
use m
    class (container), allocatable :: c1 (:)

    allocate (c1 (3))

    allocate (c1(1)%data)

    call allocateTemp (c1(1))

    call c1(1)%data%print

    deallocate (c1(1)%data)
    deallocate (c1)

    allocate (c1(2))

    call allocateTemp (c1(2))

    if (associated (c1(2)%data)) error stop 1_4

    deallocate (c1)
end

