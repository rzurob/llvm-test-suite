! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (sequence association: use
!                               array section with vec. subscript as the
!                               actual-arg)
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

program fArg028a3_1
use m
    class (base), pointer :: b1(:)

    type (child) :: c1 (10)

    c1%pos = (/(i*1.0, i=1,10)/)

    c1%name = 'xlftest 101'

    print *, 'test1'
    call print2 (c1)

    print *, 'test2'
    call print2 (c1(::3)%base)

    allocate (b1(0:9), source=c1)

    print *, 'test3'
    call print2 (b1)

    print *, 'test4'
    call print2 (b1((/8, 3, 5, 2, 3/)))
end
