! GM DTP extension using:
! ftcx_dtp -qnok -ql -qreuse=none /tstdev/F2003/abstracti/functional/abstracti012.f

!************************************************************************
!* ======================================================================
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-19 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : CLASS keyword (unlimited poly-allocatable
!                               components in intrinsic assignment; use derived
!                               type as the dynamic type)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!* =====================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(l1)    ! (20)
        integer, len :: l1
        class (*), allocatable :: data(:)
    end type
end module

module m1
    type, abstract :: dType(l2)    ! (20)
        integer, len :: l2
        contains

        procedure(printdType), pass(d), deferred :: print
    end type

    abstract interface
        subroutine printdType (d)
        import dType
            class (dType(*)), intent(in) :: d
        end subroutine
    end interface

    contains

    subroutine printData (d)
        class (dType(*)), intent(in) :: d(:)

        print *, 'size of the array:', size(d)

        do i = 1, size(d)
            call d(i)%print
        end do
    end subroutine
end module


module m2
use m1
    type, extends(dType) :: mData(l3,k1)    ! (20,20,8)
        integer, kind :: k1
        integer, len  :: l3
        integer(k1)      id

        contains

        procedure :: print => printMData
    end type

    type, extends (dType) :: nData(l4)    ! (20,20)
        integer, len  :: l4
        character(l4) :: name

        contains

        procedure :: print => printNData
    end type

    contains

    subroutine printMData (d)
        class (mData(*,*,8)), intent(in) :: d

        print *, d%id
    end subroutine

    subroutine printNData (d)
        class (nData(*,*)), intent(in) :: d

        print *, d%name
    end subroutine
end module


program abstracti012l
use m
use m2
    type (base(20)) b1(2), b2(2)

    allocate (b2(1)%data(2), source=(/mData(20,20,8)(1), mData(20,20,8)(10)/))

    allocate (b2(2)%data(3), source=(/nData(20,20)('xlftest'), nData(20,20)('team'), &
                            nData(20,20)('member')/))

    !! intrinsic assignment
    b1 = b2

    if ((.not. allocated (b1(1)%data)) .or. (.not. allocated (b1(2)%data))) &
                    error stop 1_4

    select type (x => b1(1)%data)
        class is (dType(*))
            call printData (x)
        class default
            error stop 5_4
    end select

    select type (x => b1(2)%data)
        class is (dType(*))
            call printData (x)
        class default
            error stop 6_4
    end select
end

