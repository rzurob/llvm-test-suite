! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_poly/class/fclass011a2.f
! opt variations: -qck -qnok -qnol -qreuse=base

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass011a2.f
! %VERIFY: fclass011a2.out:fclass011a2.vf
! %STDIN:
! %STDOUT: fclass011a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/21/2004
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), allocatable :: data(:)
    end type
end module

module m1
    type, abstract :: dType(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        contains

        procedure(printdType), pass(d), deferred :: print
    end type

    interface
        subroutine printdType (d)
        import dType
            class (dType(4,*)), intent(in) :: d
        end subroutine
    end interface

    contains

    subroutine printData (d)
        class (dType(4,*)), intent(in) :: d(:)

        print *, 'size of the array:', size(d)

        do i = 1, size(d)
            call d(i)%print
        end do
    end subroutine
end module


module m2
use m1
    type, extends(dType) :: mData(n3,k3)    ! (4,20,20,8)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      id

        contains

        procedure :: print => printMData
    end type

    type, extends (dType) :: nData(k4,n4)    ! (4,20,4,20)
        integer, kind :: k4
        integer, len  :: n4
        character(n4) :: name

        contains

        procedure :: print => printNData
    end type

    contains

    subroutine printMData (d)
        class (mData(4,*,*,8)), intent(in) :: d

        print *, d%id
    end subroutine

    subroutine printNData (d)
        class (nData(4,*,4,*)), intent(in) :: d

        print *, d%name
    end subroutine
end module


program fclass011a2
use m
use m2
    type (base(4,20)) b1(2), b2(2)

    allocate (b2(1)%data(2), source=(/mData(4,20,20,8)(1), mData(4,20,20,8)(10)/))

    allocate (b2(2)%data(3), source=(/nData(4,20,4,20)('xlftest'), nData(4,20,4,20)('team'), &
                            nData(4,20,4,20)('member')/))

    !! intrinsic assignment
    b1 = b2

    if ((.not. allocated (b1(1)%data)) .or. (.not. allocated (b1(2)%data))) &
                    error stop 1_4

    select type (x => b1(1)%data)
        class is (dType(4,*))
            call printData (x)
        class default
            error stop 5_4
    end select

    select type (x => b1(2)%data)
        class is (dType(4,*))
            call printData (x)
        class default
            error stop 6_4
    end select
end

