!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc001a1.f
! %VERIFY: falloc001a1.out:falloc001a1.vf
! %STDIN:
! %STDOUT: falloc001a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLCOATE (type-spec used in ALLOCATE statement;
!                               uses derived types)
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
        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        integer*4 :: id = -1

        contains

        procedure :: print => printChild
    end type

    type, extends(child) :: gen3
        character*20 :: name = 'default'

        contains

        procedure :: print => printGen3
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'empty type'
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printGen3 (b)
        class (gen3), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program falloc001a1
use m
    class (*), pointer :: x1

    class (base), pointer :: b1
    class (base), allocatable :: b2 (:)

    allocate (gen3 :: b1, x1, b2(2:3))

    call b1%print

    call b2(2)%print
    call b2(3)%print

    deallocate (b1, x1, b2)

    allocate (child :: b2(1000:1001), x1, b1)

    call b1%print

    call b2(1000)%print
    call b2(1001)%print

    deallocate (x1, b1, b2)
end
