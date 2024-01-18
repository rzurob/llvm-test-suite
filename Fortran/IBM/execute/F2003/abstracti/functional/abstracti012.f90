! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (unlimited poly-allocatable
!                               components in intrinsic assignment; use derived
!                               type as the dynamic type)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class (*), allocatable :: data(:)
    end type
end module

module m1
    type, abstract :: dType
        contains

        procedure(printdType), pass(d), deferred :: print
    end type

    abstract interface
        subroutine printdType (d)
        import dType
            class (dType), intent(in) :: d
        end subroutine
    end interface

    contains

    subroutine printData (d)
        class (dType), intent(in) :: d(:)

        print *, 'size of the array:', size(d)

        do i = 1, size(d)
            call d(i)%print
        end do
    end subroutine
end module


module m2
use m1
    type, extends(dType) :: mData
        integer(8) id

        contains

        procedure :: print => printMData
    end type

    type, extends (dType) :: nData
        character(20) :: name

        contains

        procedure :: print => printNData
    end type

    contains

    subroutine printMData (d)
        class (mData), intent(in) :: d

        print *, d%id
    end subroutine

    subroutine printNData (d)
        class (NData), intent(in) :: d

        print *, d%name
    end subroutine
end module


program abstracti012
use m
use m2
    type (base) b1(2), b2(2)

    allocate (b2(1)%data(2), source=(/mData(1), mData(10)/))

    allocate (b2(2)%data(3), source=(/nData('xlftest'), nData('team'), &
                            nData('member')/))

    !! intrinsic assignment
    b1 = b2

    if ((.not. allocated (b1(1)%data)) .or. (.not. allocated (b1(2)%data))) &
                    error stop 1_4

    select type (x => b1(1)%data)
        class is (dType)
            call printData (x)
        class default
            error stop 5_4
    end select

    select type (x => b1(2)%data)
        class is (dType)
            call printData (x)
        class default
            error stop 6_4
    end select
end

