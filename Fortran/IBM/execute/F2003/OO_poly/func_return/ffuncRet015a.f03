! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/14/2005
!*
!*  DESCRIPTION                : poly function return (deepCopy as function
!                               return)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8), pointer :: id => null()

        contains

        procedure :: getID => getBaseID
        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(20), pointer :: name => null()

        contains

        procedure :: getName => getChildName
        final :: finalizeChild
    end type

    interface base
        module procedure constructBase
    end interface

    interface child
        module procedure constructChild
    end interface

    contains

    integer(8) function getBaseID (b)
        class(base), intent(in) :: b

        if (associated(b%id)) then
            getBaseID = b%id
        else
            getBaseID = 0
        end if
    end function

    character(20) function getChildName (c)
        class(child), intent(in) :: c

        if (associated(c%name)) then
            getChildName = c%name
        else
            getChildName = ''
        end if
    end function

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        if (associated (b%id)) then
            deallocate (b%id)

            print *, 'id deallocated'
        end if
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        if (associated (c%name)) then
            deallocate (c%name)

            print *, 'name deallocated'
        end if
    end subroutine

    type(base) function constructBase (id)
        integer(8), optional, intent(in) :: id

        if (present(id)) allocate (constructBase%id, source=id)
    end function

    type(child) function constructChild (id, name)
        integer(8), optional, intent(in) :: id
        character(*), optional, intent(in) :: name

        if (present(id)) allocate (constructChild%id, source=id)
        if (present(name)) allocate (constructChild%name, source=name)
    end function

    !! make a deep copy of b
    class(base) function deepCopy (b)
        pointer deepCopy
        class(base), intent(in) :: b

        select type (b)
            type is (base)
                allocate (base::deepCopy)

                if (associated(b%id)) allocate(deepCopy%id, source=b%id)
            type is (child)
                allocate (child::deepCopy)

                if (associated(b%id)) allocate(deepCopy%id, source=b%id)

                select type (deepCopy)
                    type is (child)
                        allocate (deepCopy%name, source=b%name)
                    class default
                        error stop 11_4
                end select
            class default
                error stop 10_4
        end select
    end function
end module

program ffuncRet015a
use m
    class(base), allocatable :: b1, b2(:)

    print *, 'begin'

    allocate (b1, source=deepCopy (base(100_8)))

    print *, 'test 2'

    allocate (b2(2), source=deepCopy (child(-100_8, 'xlftest team')))

    print *, 'test 3'

    if (b1%getID() /= 100) error stop 1_4

    if (any ((/b2(1)%getID(), b2(2)%getID()/) /= -100)) error stop 2_4

    select type (b2)
        type is (child)
            if (any ((/b2(1)%getName(), b2(2)%getName()/) /= 'xlftest team')) &
                    error stop 3_4

            if ((.not. associated(b2(1)%id, b2(2)%id)) .or. &
                (.not. associated(b2(1)%name, b2(2)%name))) error stop 6_4

            nullify(b2(1)%id)
        class default
            error stop 5_4
    end select

    print *, 'test 4'

    deallocate (b1, b2)

    print *, 'end'
end