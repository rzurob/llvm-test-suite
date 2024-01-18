!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  DESCRIPTION                : argument association (large arrays as the
!                               actual-args; algothm involves forall; intended
!                               to test optimizations)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
    end type

    type, extends(base) :: child
        integer(8) :: id = -1
        character(80) :: name = 'default'
    end type

    contains

    subroutine addB (b, b1, id, name)
        class(base), intent(out) :: b(:)
        class(base), intent(in) :: b1(size(b))
        integer(8), intent(in) :: id(size(b))
        character(*), intent(in), optional :: name(size(b))

        if (.not. same_type_as (b, b1)) error stop 10_4

        select type (b)
            type is (child)
                select type (b1)
                    type is (child)
                        forall (i=1:size(b))
                            b(i)%id = b1(i)%id + id(i)

                            b(i)%name = trim(b(i)%name) // ' ' // b1(i)%name
                        end forall

                        if (present (name)) then
                            forall (i=1:size(b))
                                b(i)%name = trim(b(i)%name) // ' ' // name(i)
                            end forall
                        end if

                    class default
                        error stop 11_4
                end select
            class default
                error stop 12_4
        end select
    end subroutine
end module

program fArg040a
use m
    implicit none
    integer(8), parameter :: arraySize = 10000_8

    character(20) strings(arraySize)

    class (base), allocatable :: b1(:)
    integer i

    allocate (b1(arraySize), source=(/(child (i, name='test'), i=1, arraySize)/))

    call addB (b1(::2), b1(2::2), (/(arraySize - i, i=2,arraySize,2)/))

    !! verify results
    select type (x => b1(::2))
        type is (child)
            if (any(x%id /= arraySize)) error stop 1_4

            if (any(x%name /= 'default test')) error stop 2_4
        class default
            error stop 3_4
    end select

    !! 2nd test
    strings = 'again'

    call addB (b1(::2), b1(2::2), (/(arraySize - i, i=2,arraySize,2)/), strings)

    !! verify results
    select type (x => b1(::2))
        type is (child)
            if (any(x%id /= arraySize)) error stop 5_4

            if (any(x%name /= 'default test again')) error stop 6_4
        class default
            error stop 7_4
    end select
end
