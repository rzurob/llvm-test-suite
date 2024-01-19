! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/definedOp003.f
! opt variations: -qck -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/19/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the elemental defined operator // for a
!                               derived type with deferred char length
!                               component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type string(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        character(:), allocatable :: str

        contains

        procedure :: concat => concatString
        generic :: operator(//) => concat
    end type

    contains

    type (string(4,20)) elemental function concatString (s, c)
        class(string(4,*)), intent(in) :: s
        character(*), intent(in) :: c

        if (allocated(s%str)) then
            concatString%str = s%str // c
        else
            concatString%str = c
        end if
    end function
end module

program definedOp003
use m
    type(string(4,:)), allocatable :: s1(:,:,:), s2(:,:,:)

    character(:), allocatable :: c1(:,:,:)

    allocate (string(4,20) :: s2(0:4,0:5,0:1))

    !! test 1: string component not allocated
    s1 = s2 // 'xlftest'

    if (.not. allocated(s1)) error stop 1_4

    if (any(lbound(s1) /= 1) .or. any(ubound(s1) /= (/5,6,2/))) error stop 2_4

    do i = 1, 5
        do j = 1, 6
            do k = 1, 2
                if ((.not. allocated(s1(i,j,k)%str)) .or. &
                    (len(s1(i,j,k)%str) /= 7)) error stop 3_4

                if (s1(i,j,k)%str /= 'xlftest') error stop 4_4
            end do
        end do
    end do

    !! test 2: string components are allocated
    c1 = reshape ((/(' ' // achar(64+i), i=1, 10)/), (/2,2,2/))

    s2 = s1((/1,3/), (/2,4/), :) // c1

    if (any(lbound(s2) /= 1) .or. any(ubound(s2) /= 2)) error stop 5_4

    itotal = 1

    do k = 1, 2
        do j = 1, 2
            do i = 1, 2
                if ((.not. allocated(s2(i,j,k)%str)) .or. &
                    (len(s2(i,j,k)%str) /= 9)) error stop 6_4

                if (s2(i,j,k)%str /= 'xlftest ' // achar(64+itotal)) &
                    error stop 7_4

                itotal = itotal + 1
            end do
        end do
    end do
end
