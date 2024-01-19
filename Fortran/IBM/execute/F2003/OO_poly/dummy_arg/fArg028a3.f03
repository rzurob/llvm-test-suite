! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (sequence association for
!                               rank-two dummy-arg array)
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
        real(4) :: pos

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

        write (*, '(f10.2)') b%pos
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print '(f10.2,a,a)', b%pos, ', name=', b%name
    end subroutine

    subroutine print2 (b)
        class (base) b(0:1,2)

        call b(0,1)%print
        call b(0,2)%print
    end subroutine

    subroutine print2X (x)
        class (*) x(2,2)
    end subroutine
end module

program fArg028a3
use m
    class (base), allocatable :: b1(:)

    type (child) :: c1 (10)

    c1%pos = (/(i*1.0, i=1,10)/)

    c1%name = 'xlftest 101'

    call print2 (c1(3))

    allocate (b1(0:9), source=c1)

    call print2 (b1((/5,6,7,8,7/)))   !<-- same as call print2(c1(6))
end
