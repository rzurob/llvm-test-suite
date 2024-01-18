! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/allocEnh/argAssociation/dummyArg008a1.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/19/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               array case for test case dummyArg008a.f
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind            :: k1
        integer, len             :: n1
        complex(k1), allocatable :: cx(:)

        contains

        procedure, pass(b) :: print => printBase
    end type

    type, extends(base) :: child    ! (20,8)
        character(:), pointer :: names(:)

        contains

        procedure, pass(b) :: print => printChild
    end type

    contains

    integer function printBaseCxAllocStatus (unit, b)
        class(base(*,8)), intent(in) :: b
        integer, intent(in) :: unit

        allocatable printBaseCxAllocStatus

        if (allocated(b%cx)) then
            write (unit, *) 'cx bounds: ', lbound(b%cx), ubound(b%cx)

            printBaseCxAllocStatus = 1
        else
            write (unit, *) 'cx not allocated'

            printBaseCxAllocStatus = 0
        end if
    end function

    subroutine printBase (unit, b, fmt)
        class(base(*,8)), intent(in) :: b
        integer, intent(in) :: unit
        character(*), intent(in) :: fmt

        if (printBaseCxAllocStatus(unit,b) == 1)  write (unit, fmt) b%cx
    end subroutine

    subroutine printChild (unit, b, fmt)
        class(child(*,8)), intent(in) :: b
        integer, intent(in) :: unit
        character(*), intent(in) :: fmt

        if (printBaseCxAllocStatus(unit,b) == 1) then
            if (associated(b%names)) then
                write (unit, *) 'names bounds', lbound(b%names), ubound(b%names)
                write (unit, fmt) b%cx, b%names
            else
                write (unit, fmt) b%cx, 'NULL'
            end if
        end if
    end subroutine
end module


program dummyArg008a
use m
use, intrinsic :: iso_fortran_env, only: error_unit
    interface assgn
        subroutine assgnBase (b1, b2)
        import base
            class(base(:,8)), allocatable, intent(out) :: b1(:)
            class(base(*,8)) b2(:)
        end subroutine
    end interface

    type container(k2,n2,k3)    ! (4,20,8)
        integer, kind                  :: k2,k3
        integer, len                   :: n2
        class(base(:,k3)), allocatable :: data(:)
    end type

    type(container(4,:,8)), allocatable :: co1, co2

    character(20), target :: str(10)

    str = 'test'

    co1 = container(4,20,8)(null())

    co2 = container(4,20,8)([(child(20,8)([(cmplx(i,i-1), i=1,10)], str), j=1,2)])

    call assgn (co1%data, co2%data)

    call co1%data(1)%print(error_unit, '(10("(",f15.6, f15.6,") "), /, 10a)')
    call co1%data(2)%print(error_unit, '(10("(",f15.6, f15.6,") "), /, 10a)')

    call assgn (co1%data, [base(20,8)(null()), &
        base(20,8)([(1.2d0, 2.2d0), (3.2d0, 4.2d0)]), base(20,8)(null())])

    call co1%data(1)%print(error_unit, '(4f12.4)')
    call co1%data(2)%print(error_unit, '(4f12.4)')
    call co1%data(3)%print(error_unit, '(4f12.4)')
end


subroutine assgnBase (b1, b2)
use m
    class(base(:,8)), allocatable, intent(out) :: b1(:)
    class(base(*,8)) b2(:)

    if (same_type_as (b2, base(20,8)(null()))) then
        allocate(base(20,8):: b1(size(b2)))
    else if (same_type_as (b2, child(20,8)(null(), null()))) then
        allocate (child(20,8):: b1(size(b2)))
    else
        stop 10
    end if

    select type (b1)
        type is (base(*,8))
            b1 = b2

        type is (child(*,8))
            select type (b2)
                type is (child(*,8))
                    b1 = b2

                class default
                    stop 15
            end select

        class default
            stop 11

    end select
end subroutine
