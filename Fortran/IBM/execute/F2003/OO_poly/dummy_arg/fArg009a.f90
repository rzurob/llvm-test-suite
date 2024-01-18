!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg009a.f
! %VERIFY: fArg009a.out:fArg009a.vf
! %STDIN:
! %STDOUT: fArg009a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (If the dummy-arg has the
!                               VALUE attribute it becomes associated with a
!                               definable anonymous data whose initial value is
!                               that of the actual argument.)
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
        integer*4, pointer :: id => null()

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type


    interface printData
        subroutine printData (b)
        import
            type (base), value :: b
        end subroutine

        subroutine printDataChild (b)
        import
            type (child), value :: b
        end subroutine
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (associated (b%id)) then
            print *, b%id
        else
            print *, 'null'
        end if
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        call b%base%print
        print *, b%name
    end subroutine

    subroutine printDataPoly (b)
        class(base) :: b

        select type (b)
            class is (child)
                call printData(b)

            class default
                call printData(b)

        end select
    end subroutine
end module

program fArg009a
use m
    class (base), allocatable :: b1, b2, b3
    integer*4, target :: i1 = 10

    allocate (b1, source=child(null(), 'b1'))
    allocate (b2, source = child (i1, 'b2'))

    allocate (b3)

    allocate (b3%id)

    b3%id = 100

    call printDataPoly (b1)

    call printDataPoly (b2)

    call printDataPoly (b3)
end


subroutine printData (b)
use m, only: base
    type (base), value :: b

    call b%print
end subroutine

subroutine printDataChild (b)
use m, only: child
    type (child), value :: b

    call b%print
end subroutine
