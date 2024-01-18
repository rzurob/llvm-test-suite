! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg038.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2005
!*
!*  DESCRIPTION                : argument association (VALUE attribute and
!                               function results as the actual-arg)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (4,8)
        integer, kind :: k2
        real(k2)         value

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class(base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class(child(4,8)), intent(in) :: b

        write (*, '(i10,f12.2)') b%id, b%value
    end subroutine

    subroutine printB1 (b)
        type(base(4)), value :: b

        call printB (b)
    end subroutine

    subroutine printC1 (b)
        type(child(4,8)), value :: b

        call printB (b)
    end subroutine

    recursive subroutine printB (b)
        class (base(4)) :: b

        logical :: pass = .false.

        if (.not. pass) then
            pass = .true.

            select type (b)
                type is (child(4,8))
                    call printC1 (b)

                class default
                    call printB1(b)
            end select
        else
            pass = .false.

            call b%print
        end if

    end subroutine

    class(base(4)) function makeData (id, value)
        integer, value :: id
        real(8), optional, value :: value
        allocatable makeData

        if (present(value)) then
            allocate (makeData, source=child(4,8)(id, value))
        else
            allocate (makeData, source=base(4)(id))
        end if
    end function

    type (base(4)) function makeBaseObj (id)
        integer, value :: id

        makeBaseObj%id = id
    end function

    type (child(4,8)) function makeChildObj (id, value)
        integer, value :: id
        real(8), value :: value

        makeChildObj%id = id
        makeChildObj%value = value
    end function
end module

program fArg038
use m
    !! structure constructor
    call printB (base(4)(10))
    call printB (child(4,8)(100, 1.5_8))

    !! poly-function results
    call printB (makeData (20))
    call printB (makeData (200, 3.1_8))

    !! non-poly function result
    call printB (makeBaseObj (300))
    call printB (makeChildObj (30, 5.2_8))
end
