! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg513.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (base(4)), pointer :: x

    contains

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine createData (b, func, id, name)
        class (base(4)), pointer, intent(out) :: b
        integer*4, intent(in) :: id
        character(*), optional, intent(in) :: name

        interface
            function func (id, name)
            import base
                class (base(4)), pointer :: func
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
            class (base(4)), pointer :: createBasePtr1
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
    class (base(4)), pointer :: createBasePtr1
    integer*4, intent(in) :: id
    character(*), intent(in), optional :: name

    if (present (name)) then
        allocate (createBasePtr1, source=child(4,1,20) (id, name))
    else
        allocate (createBasePtr1, source=base(4) (id))
    end if
end function
