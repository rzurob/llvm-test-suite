! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/11/2005
!*
!*  DESCRIPTION                : argument association (sorting algorithm as the
!                               actual-arg for a type-bound)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    interface
        logical function less (b1, b2)
        import base
            class (base), intent(in) :: b1, b2
        end function
    end interface

    interface assignment(=)
        subroutine b1AssgnByB2 (b1, b2)
        import base
            class (base), intent(out) :: b1
            class (base), intent(in) :: b2
        end subroutine
    end interface

    type table
        class (base), allocatable :: data (:)

        contains

        procedure :: print => printTable
        procedure :: sort => sortWithAlg
    end type

    contains

    subroutine printBase (b)
        class (base), intent (in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printTable (t)
        class (table), intent(in) :: t

        if (allocated (t%data)) then
            do i = 1, size (t%data)
                call t%data(i)%print
            end do
        end if
    end subroutine

    subroutine sortWithAlg (t, extern_less)
        class (table), intent(inout) :: t

        procedure (less) extern_less

        class (base), allocatable :: temp

        if (allocated (t%data)) then
            select type (x => t%data)
                type is (base)
                    allocate (temp)
                type is (child)
                    allocate (child:: temp)
                class default
                    error stop 50_4
            end select


            !! use bubble sort
            do i = 1, size (t%data)
                do j = 1, size(t%data) - i
                    if (extern_less(t%data(j+1), t%data(j))) then
                        temp = t%data(j+1)
                        t%data(j+1) = t%data(j)
                        t%data(j) = temp
                    end if
                end do
            end do
        end if

    end subroutine
end module


subroutine b1AssgnByB2 (b1, b2)
use m, only: base, child
    class (base), intent(out) :: b1
    class (base), intent(in) :: b2

    if (.not. same_type_as (b1, b2)) error stop 30_4

    select type (b1)
        type is (base)
            b1%id = b2%id
        type is (child)
            select type (b2)
                type is (child)
                    b1 = b2
                class default
                    error stop 31_4
            end select
        class default
            error stop 32_4
    end select
end subroutine

program fArg513a
use m
    class (table), pointer :: t1(:)
    class (base), allocatable :: b1(:)

    procedure (less) sortWithID, sortWithIDName

    allocate (t1(2))

    allocate (child:: b1(10))

    b1%id = (/3, 2, 2, 2, 4, 5, 3, 4, 9, 2/)

    select type (b1)
        type is (child)
            b1(1)%name = 'abc_3.1'
            b1(2)%name = 'abc_2.1'
            b1(3)%name = 'xyz_2.2'
            b1(4)%name = 'ibm_2.3'
            b1(5)%name = '123_4.1'
            b1(6)%name = 'nowhere_5'
            b1(7)%name = 'ABC_3.2'
            b1(8)%name = 'mbi_4.2'
            b1(9)%name = 'biggest_9'
            b1(10)%name = 'SMALL_2.4'

        class default
            error stop 1_4
    end select

    allocate (t1(1)%data(size(b1)), source=b1)

    call t1(1)%sort(sortWithID)
    call t1(1)%print

    print *, new_line('a'), 'second test', new_line('a')

    call t1(1)%sort (extern_less=sortWithIDName)

    call t1(1)%print
end


logical function sortWithID (b1, b2)
use m, only : base
    class (base), intent(in) :: b1, b2

    if (.not. same_type_as (b1, b2)) error stop 10_4

    sortWithID = (b1%id < b2%id)
end function


logical function sortWithIDName (b1, b2)
use m, only: base, child
    class(base), intent(in) :: b1, b2

    select type (b1)
        type is (base)
            sortWithIDName = IDcmp (b1, b2)
        type is (child)
            select type (b2)
                type is (child)
                    sortWithIDName = IDcmp (b1, b2)

                    if ((.not. sortWithIDName) .and. (b1%id == b2%id)) then
                        sortWithIDName = b1%name < b2%name
                    end if
                class default
                    error stop 21_4
            end select
        class default
            error stop 22_4
    end select

    contains

    logical function IDcmp (bb1, bb2)
        class (base), intent(in) :: bb1, bb2

        if (.not. same_type_as (bb1, bb2)) error stop 20_4

        IDcmp = bb1%id < bb2%id
    end function
end function
