!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg513.f
! %VERIFY: fArg513.out:fArg513.vf
! %STDIN:
! %STDOUT: fArg513.out
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
!*  DESCRIPTION                : argument association (dummy procedure for the
!                               dummy-arg)
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

    class (base), pointer :: x

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine createData (b, func, id, name)
        class (base), pointer, intent(out) :: b
        integer*4, intent(in) :: id
        character(*), optional, intent(in) :: name

        interface
            function func (id, name)
            import base
                class (base), pointer :: func
                integer*4, intent(in) :: id
                character(*), intent(in), optional :: name
            end function
        end interface

        b => func (id, name)

    end subroutine
end module

program fArg513
use m
    interface
        function createBasePtr1 (id, name)
            use m
            class (base), pointer :: createBasePtr1
            integer*4, intent(in) :: id
            character(*), intent(in) :: name
        end function
    end interface

    call createData (x, func = createBasePtr1, id = 10, name = 'child_type')

    call x%print

    deallocate (x)

    call createData (x, createBasePtr1, id = -1)

    call x%print

    deallocate (x)
end

function createBasePtr1 (id, name)
use m
    class (base), pointer :: createBasePtr1
    integer*4, intent(in) :: id
    character(*), intent(in), optional :: name

    if (present (name)) then
        allocate (createBasePtr1, source=child (id, name))
    else
        allocate (createBasePtr1, source=base (id))
    end if
end function
