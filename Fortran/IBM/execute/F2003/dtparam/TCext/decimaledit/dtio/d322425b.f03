! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/decimaledit/dtio/d322425b.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/07/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 322425)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: x(:)
    end type

    type base(k2,n2)    ! (4,20)
        integer, kind                      :: k2
        integer, len                       :: n2
        class(dataType(k2,:)), allocatable :: data
    end type
end module

program dcmlChildWrite005
use m
    type(base(4,:)), allocatable :: b1(:)
    logical(4), external :: precision_r4

    i = 1

    allocate (base(4,20) :: b1(1))
    allocate (dataType(4,20) :: b1(1)%data)
    allocate (b1(1)%data%x(1), source=base(4,20)(dataType(4,20)(sin((/(j*1.0, j=1, i+2)/)))))

    select type (x => b1(1)%data%x(1))
        type is (base(4,*))
            select type (y => x%data%x)
                type is (real)
                    if (size(y) /= 3) error stop 1

                    do i = 1, 3
                        if (.not. precision_r4(y(i), sin(i*1.0))) error stop 2
                    end do

                class default
                    stop 20
            end select

        class default
            stop 10
    end select
end
