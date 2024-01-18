! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/intrinsic005.f
! opt variations: -qnock -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/06/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test MERGE for derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: id
    end type

    type, extends(base) :: child    ! (20,8)
        character(:), allocatable :: name
    end type

    interface base
        procedure baseObjWithDefault
    end interface

    interface child
        procedure childObjWithDefault
    end interface

    contains

    type(base(20,8)) function baseObjWithDefault ()
        baseObjWithDefault%id = -1
    end function

    type(child(20,8)) function childObjWithDefault ()
        childObjWithDefault%base = base()

        childObjWithDefault%name = 'no-name'
    end function
end module

program intrinsic005
use m
    type(base(:,8)), allocatable :: b1(:), b2(:), b3(:)

    type(child(:,8)), allocatable :: c1(:,:), c2(:,:)

    logical, allocatable :: mask(:,:)

    allocate (base(20,8) :: b1(10), b2(0:9))
    allocate (child(20,8) :: c1(10, 20))
    allocate (mask(0:9, 0:19))

    do i = 1, 10
        b1(i)%id = i

        b2(i-1)%id = i*100
    end do

    do i = 1, 10
        do j = 1, 20
            c1(i,j)%id = i*j

            c1(i,j)%name = 'xlftest'

            mask (i-1,j-1) = mod(i*j, 2) == 0
        end do
    end do

    b3 = merge (b1, b2, [(mod(i,2) == 0, i=0,9)])

    c2 = merge (c1, child(), mask)

    !! verify b3
    do i = 1, 10, 2
        if ((.not. allocated(b3(i)%id)) .or. (.not. allocated(b3(i+1)%id))) &
                error stop 1_4

        if (b3(i)%id /= i) error stop 2_4

        if (b3(i+1)%id /= 100*(i+1)) error stop 3_4
    end do

    !verify c2
    do i = 1, 10
        do j = 1, 20
            if ((.not. allocated(c2(i,j)%id)) .or. &
                (.not. allocated(c2(i,j)%name))) error stop 4_4

            if (mod(i*j,2) == 0) then
                if (c2(i,j)%id /= i*j) error stop 5_4

                if (c2(i,j)%name /= 'xlftest') error stop 6_4
            else
                if (c2(i,j)%id /= -1) error stop 7_4

                if (c2(i,j)%name /= 'no-name') error stop 8_4
            end if
        end do
    end do
end
