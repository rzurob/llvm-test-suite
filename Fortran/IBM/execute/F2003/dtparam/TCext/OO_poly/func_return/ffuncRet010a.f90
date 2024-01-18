! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet010a.f
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
!*  DATE                       : 05/06/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly function return (poly pointer scalar
!                               function result used in the defined assignment)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind            :: k1
        integer(k1), allocatable :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface makeData
        module procedure makeBasePtr
    end interface

    contains

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        if (allocated (b%id)) then
            print *, b%id
        else
            print *, 'id not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child(8,1,*)), intent(in) :: b

        if (allocated (b%id)) then
            print *, b%id, b%name
        else
            print *, 'id not allocated; ', b%name
        end if
    end subroutine

    class(base(8)) function makeBasePtr (id, name)
        pointer makeBasePtr
        integer(8), intent(in), allocatable :: id
        character(*), intent(in), optional :: name

        if (present(name)) then
            allocate (makeBasePtr, source=child(8,1,20)(id, name))
        else
            allocate (makeBasePtr, source=base(8)(id))
        end if
    end function
end module

module m1
use m
    type container(k3)    ! (8)
        integer, kind            :: k3
        class(base(k3)), pointer :: data => null()

        contains

        final :: finalizeContainer
    end type

    interface assignment(=)
        module procedure assgnCo1Co2
    end interface

    contains

    subroutine assgnCo1Co2 (co1, co2)
        class(container(8)), intent(out) :: co1
        class(container(8)), intent(in) :: co2

        if (associated(co2%data)) then
            allocate (co1%data, source=co2%data)
        end if
    end subroutine

    subroutine finalizeContainer (co)
        type (container(8)), intent(inout) :: co

        if (associated(co%data)) then
            print *, 'deallocating data'

            deallocate (co%data)
        end if
    end subroutine
end module

program ffuncRet010a
use m1
    class (container(8)), allocatable :: co1
    integer(8), allocatable :: id

    allocate (id, source=-10_8)

    allocate (co1)

    co1 = container(8) (makeData(id))

    if (.not. associated(co1%data)) error stop 1_4

    call co1%data%print

    co1 = container(8) (makeData(id, 'xlftest'))

    if (.not. associated(co1%data)) error stop 2_4

    call co1%data%print
end
