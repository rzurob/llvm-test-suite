! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/allocEnh/construct/associate005.f
! opt variations: -qnok -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/08/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the case where selector is a function
!                               result from elemental function.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: data

        contains

        procedure :: filter => genBase
    end type

    contains

    elemental function genBase (b, b2)
        class(base(4,*)), intent(in) :: b, b2

        type(base(4,20)) genBase

        if ((.not. allocated(b%data)) .or. (.not. allocated(b2%data))) return

        if (same_type_as(b%data, b2%data)) genBase = base(4,20)(b2%data)
    end function
end module

program associate005
use m
    class(base(4,:)), allocatable :: b1(:,:)
    type(base(4,:)), allocatable :: b2, b3(:,:)

    b2 = base(4,20)(1)

    allocate (b1(3,3), source=reshape((/(base(4,20)(i*1.2), i=1,3), &
        (base(4,20)(i), i=1,3), (base(4,20)(i*1_8), i=1,3)/), (/3,3/)))


    associate (x => b2%filter(b1), y => b1%filter(b2))
        b3 = x

        if ((.not. allocated(b3)) .or. any(shape(b3) /= (/3,3/))) error stop 1_4
        do i = 1, 3
            if (allocated(b3(i,1)%data) .or. allocated(b3(i,3)%data)) &
                error stop 2_4

            if (.not. allocated(b3(i,2)%data)) error stop 3_4

            select type (u => b3(i,2)%data)
                type is (integer)
                    u = u + 10

                class default
                    error stop 5_4
            end select
        end do

        !! the following line will NOT change values of b3
        b3 = y%filter(b3)

        if ((.not. allocated(b3)) .or. any(shape(b3) /= (/3,3/))) error stop 6_4
        do i = 1, 3
            if (allocated(b3(i,1)%data) .or. allocated(b3(i,3)%data)) &
                error stop 7_4

            if (.not. allocated(b3(i,2)%data)) error stop 8_4

            select type (u => b3(i,2)%data)
                type is (integer)
                    if (u /= 10+i) error stop 9_4

                class default
                    error stop 10_4
            end select
        end do
    end associate
end
