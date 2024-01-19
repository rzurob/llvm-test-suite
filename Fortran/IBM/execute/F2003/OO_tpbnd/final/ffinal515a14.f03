! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/14/2005
!*
!*  DESCRIPTION                : final sub (finalization of temp created by
!                               structure constructor in where statement)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: id

        contains

        procedure :: equal => b1EqualB2
        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(10) :: name
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        if (allocated (b%id)) then
            print *, 'deallocate id'

            deallocate (b%id)
        end if
    end subroutine

    elemental logical function b1EqualB2 (b1, b2)
        class (base), intent(in) :: b1, b2

        !! first compare the base part
        if (.not. same_type_as (b1, b2)) then
            b1EqualB2 = .false.
            return
        end if

        if (allocated (b1%id)) then
            b1EqualB2 = allocated (b2%id)

            if (b1EqualB2) then
                b1EqualB2 = (b1%id == b2%id)
            end if
        else
            b1EqualB2 = .not. allocated (b2%id)
        end if

        !! then test the extended part
        if (b1EqualB2) then
            select type (b1)
                type is (base)
                type is (child)
                    select type (b2)
                        type is (child)
                            b1EqualB2 = (b1%name == b2%name)
                        class default
                            b1EqualB2 = .false. !<-- should never be here
                    end select
                class default
                    b1EqualB2 = .false.
            end select
        end if
    end function
end module

program ffinal515a14
use m
    class (base), allocatable :: b1(:,:)

    logical results(2,2)

    results = .false.

    allocate (child :: b1(2,2))

    allocate (b1(1,1)%id, source=1)
    allocate (b1(2,2)%id, source=1)
    allocate (b1(2,1)%id, source=2)

    select type (b1)
        type is (child)
            b1(1,1)%name = 'test'
            b1(2,2)%name = 'xlftest'
            b1(2,1)%name = 'test'
        class default
            error stop 1_4
    end select

    where (b1%equal(child(1, 'test')))   results = .true.

    print *, results
end
