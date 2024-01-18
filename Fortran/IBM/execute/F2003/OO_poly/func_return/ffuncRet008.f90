!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/25/2005
!*
!*  DESCRIPTION                : poly function result (use of the RESULT
!                               keyword)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id

        contains

        procedure, non_overridable :: makePtr => makeBasePtr
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    contains

    function makeBasePtr(b) result (ptr)
        class (base), intent(in) :: b
        class (base), pointer :: ptr

        allocate (ptr, source=b)
    end function
end module

program ffuncRet008
use m
    class (base), pointer :: p1

    type (child) c1

    c1 = child(1, 'test')

    p1 => c1%makePtr()

    select type (p1)
        type is (child)
            if (p1%id /= 1) error stop 1_4

            if (p1%name /= 'test') error stop 2_4
        class default
            error stop 5_4
    end select

    deallocate (p1)
end
