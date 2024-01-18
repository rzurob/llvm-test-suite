!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg028a.f
! %VERIFY: fArg028a.out:fArg028a.vf
! %STDIN:
! %STDOUT: fArg028a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (a test on sequence
!                               association)
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
    end type

    type, extends(base) :: child
        class (base), pointer :: data => null()

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    recursive subroutine printChild (b)
        class (child), intent(in) :: b

        if (associated (b%data)) then
            print *, 'id =', b%id, ', and data is:'
            call b%data%print
        else
            print *, 'id =', b%id, ', data is null'
        end if
    end subroutine

    subroutine printVal (b)
        class (base), intent(in) :: b(3)

        call b(1)%print
        call b(2)%print
        call b(3)%print
    end subroutine
end module

program fArg028a
use m
    type (child), target :: c1 (3)

    type (base), target :: b1 (2)
    class (base), pointer :: c2, c3(:)


    b1 = (/base(10), base(20)/)

    allocate (c2, source=child(30, b1(1)))


    c1 = (/child(1, b1(2)), child(2), child(3,c2)/)

    allocate (c3(3), source=c1)


    call printVal ((/c2, c3(2:3)/))

    deallocate (c2, c3)
end
