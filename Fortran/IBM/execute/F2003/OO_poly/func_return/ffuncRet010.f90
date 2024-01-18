!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/05/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly-function return (poly pointer function
!                               return used in defined operator)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8), allocatable :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    interface makeData
        module procedure makeBasePtr
    end interface

    contains

    class (base) function makeBasePtr (id, name, lb, ub)
        pointer makeBasePtr(:)
        integer(8), allocatable, intent(in) :: id
        character(*), intent(in), optional :: name
        integer, intent(in) :: lb, ub

        if (.not. present(name)) then
            allocate (makeBasePtr(lb:ub), source=base(id))
        else
            allocate (makeBasePtr(lb:ub), source=child(id, name))
        end if
    end function

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (allocated(b%id)) then
            print *, b%id
        else
            print *, 'id not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        if (allocated(b%id)) then
            print *, b%id, ';', b%name
        else
            print *, 'id not allocated;', b%name
        end if
    end subroutine
end module

module m1
use m
    type container
        class (base), pointer :: data(:) => null()

        contains

        final :: finalizeContainer
        procedure :: print => printContainer
    end type

    interface assignment(=)
        module procedure assgnB1B2
    end interface

    contains

    subroutine assgnB1B2 (b1, b2)
        class (container), intent(out) :: b1
        class (container), intent(in) :: b2

        if (associated(b2%data)) then
            allocate (b1%data(lbound(b2%data,1):ubound(b2%data,1)), source= &
                    b2%data)
        end if
    end subroutine

    subroutine printContainer (co)
        class (container), intent(in) :: co

        if (associated(co%data)) then
            do i = lbound(co%data,1), ubound(co%data,1)
                call co%data(i)%print
            end do
        end if
    end subroutine

    subroutine finalizeContainer (co)
        type (container), intent(inout) :: co

        if (associated (co%data)) then
            print *, 'deallocating data'

            deallocate (co%data)
        end if
    end subroutine
end module

program ffuncRet010
use m1
    class (container), allocatable :: co1
    integer(8), allocatable :: id

    allocate (id, source=100_8)

    allocate (co1)

    co1 = container (makeData(id, lb=0, ub=2))

    call co1%print

    co1 = container (makeData(id, 'xlftest', -1, 0))

    call co1%print
end
