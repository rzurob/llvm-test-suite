! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/F2003/decimaledit/dtio/d322425.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 322425)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType(k1)    ! (4)
        integer, kind :: k1
        class(*), allocatable :: x(:)
    end type

    type base(k2)    ! (4)
        integer, kind                    :: k2
        class(dataType(k2)), allocatable :: data
    end type
end module

program dcmlChildWrite005
use m
    type(base(4)), allocatable :: b1(:)

    allocate (b1(12))

    b1(3) = base(4)(dataType(4)((/base(4)(dataType(4)(null()))/)))

    select type (x => b1(3)%data%x)
        type is (base(4))
            if (size(x) /= 1) stop 1

            if (allocated(x(1)%data%x)) stop 2
        class default
            stop 10
    end select
end
