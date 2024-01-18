! GB DTP extension using:
! ftcx_dtp -qck -ql -qnodefaultpv -qnodeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn006a3.f
! opt variations: -qnock -qnol -qdefaultpv -qdeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2005
!*
!*  DESCRIPTION                : data pointer assignment (data pointer
!                               assignment for components during the intrinsic
!                               assignment for the derived type; use the private
!                               pointer components)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind        :: k1
        integer, len         :: n1
        integer(k1), private :: id

        contains

        procedure :: whatID => getBaseID
        procedure :: assgnID => assgnBaseID
    end type

    type, extends (base) :: child(k2)    ! (20,8,1)
        integer, kind                      :: k2
        character(kind=k2,len=n1), private :: name

        contains

        procedure :: whatName => getChildName
        procedure :: assgnName => assgnChildName
    end type

    contains

    integer(8) function getBaseID (b)
        class (base(*,8)), intent(in) :: b

        getBaseID = b%id
    end function

    subroutine assgnBaseID (b, id)
        class (base(*,8)), intent(inout) :: b
        integer(8), intent(in) :: id

        b%id = id
    end subroutine

    character(20) function getChildName (c)
        class (child(*,8,1)), intent(in) :: c

        getChildName = c%name
    end function

    subroutine assgnChildName (c, name)
        class (child(*,8,1)), intent(inout) :: c
        character(*), intent(in) :: name

        c%name = name
    end subroutine
end module


module m1
use m
    type container(n2,k3)    ! (20,8)
        integer, kind                        :: k3
        integer, len                         :: n2
        class(base(n2,k3)), pointer, private :: data1
        class(base(n2,k3)), pointer, private :: data2(:)

        contains

        procedure :: print => printContainer
        procedure :: assgnData1 => assgnContainerData1
        procedure :: assgnData2 => assgnContainerData2
        procedure, private, nopass :: printBase
    end type

    contains

    subroutine printBase (b)
        class (base(*,8)), intent(in) :: b

        select type (b)
            type is (base(*,8))
                print *, b%whatID()
            type is (child(*,8,1))
                print *, b%whatID(), b%whatName()
            class default
                error stop 10_4
        end select
    end subroutine

    subroutine printContainer (co)
        class (container(*,8)), intent(in) :: co

        if (associated (co%data1)) then
            print *, 'data1 associated'
            call co%printBase (co%data1)
        else
            print *, 'data1 not associated'
        end if

        if (associated (co%data2)) then
            print *, 'data2 associated'

            do i = lbound(co%data2,1), ubound(co%data2,1)
                call co%printBase(co%data2(i))
            end do
        else
            print *, 'data2 not associated'
        end if
    end subroutine

    subroutine assgnContainerData1 (co, b1)
        class (container(*,8)), intent(inout) :: co
        class (base(*,8)), intent(in) :: b1

        allocate (co%data1, source=b1)
    end subroutine

    subroutine assgnContainerData2 (co, b2)
        class (container(*,8)), intent(inout) :: co
        class (base(*,8)), intent(in) :: b2(:)

        allocate (co%data2(size(b2)), source=b2)
    end subroutine
end module

program fpAssgn006a3
use m1
    class (base(20,8)), allocatable :: b1, b2(:)
    type (container(20,8)) :: co1, co2

    allocate (b1)
    allocate (child(20,8,1) :: b2(10))

    call b1%assgnID (10_8)

    do i = 1, 10
        call b2(i)%assgnID (100_8+i)
    end do

    select type (b2)
        type is (child(*,8,1))
            do i = 1, 10
                call b2(i)%assgnName ('xlf test_'//char(ichar('0')+i-1))
            end do
        class default
            error stop 2_4
    end select

    call co2%assgnData1 (b1)
    call co2%assgnData2 (b2)

    !! we're testing the intrinsic assignment
    co1 = co2

    call co1%print
end
