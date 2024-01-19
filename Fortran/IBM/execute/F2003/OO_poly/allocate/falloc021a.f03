! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (for pointer function result, it can
!                               be disassociated)
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
        integer(2) :: id

        contains

        procedure :: print => printBase
        procedure, nopass :: printType => printBaseType
    end type

    type, extends(base) :: child
        character(15) :: name

        contains

        procedure :: print => printChild
        procedure, nopass :: printType => printChildType
    end type

    interface genData
        class (base) function genBasePtr (id)
        import base
            integer(4), intent(in), optional :: id
            pointer genBasePtr
        end function

        class (child) function genChildPtr (id, name)
        import child
            integer(4), intent(in), optional :: id
            character (*), intent(in) :: name
            pointer genChildPtr
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printBaseType
        print *, 'base'
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printChildType
        print *, 'child'
    end subroutine
end module


class (base) function genBasePtr (id)
use m, only: base
    integer(4), intent(in), optional :: id
    pointer genBasePtr

    if (present (id)) then
        allocate (genBasePtr, source=base(id))
    else
        genBasePtr => null()
    end if
end function


class (child) function genChildPtr (id, name)
use m, only : child
    integer(4), intent(in), optional :: id
    character(*), intent(in) :: name
    pointer genChildPtr

    if (present (id)) then
        allocate (genChildPtr, source = child(id, name))
    else
        nullify (genChildPtr)
    end if
end function


program falloc021a
use m
    class (base), pointer :: b1_ptr

    b1_ptr => genData (10)

    call b1_ptr%print

    deallocate (b1_ptr)

    b1_ptr => genData ()

    if (associated (b1_ptr)) error stop 1_4

    b1_ptr => genData (1, 'test1')

    call b1_ptr%print

    deallocate (b1_ptr)

    b1_ptr => genData (name = 'abc')

    if (associated (b1_ptr)) error stop 2_4

    call b1_ptr%printType
end
