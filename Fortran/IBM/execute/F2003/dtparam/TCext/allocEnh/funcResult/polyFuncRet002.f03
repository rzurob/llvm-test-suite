! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/polyFuncRet002.f
! opt variations: -qck -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/26/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               A poly function return is an array and used as a
!                               target for the poly-pointer bounds-remapped;
!                               used in the intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind     :: k1
        integer, len      :: n1
        real(k1), pointer :: data => null()
    end type

    type, extends(base) :: child    ! (20,4)
        character(:), pointer :: str => null()
    end type

    type string(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(:), allocatable :: str
    end type

    interface genArray
        procedure genBaseArray
        procedure genChildArray
    end interface

    contains

    class (base(:,4)) function genBaseArray (b1)
        real, intent(in) :: b1(:)

        pointer genBaseArray(:)

        allocate (base(20,4) :: genBaseArray(size(b1)))

        do i = 1, size(b1)
            allocate (genBaseArray(i)%data, source=b1(i))
        end do
    end function


    class(base(:,4)) function genChildArray (b1, c1)
        real, intent(in) :: b1(:)
        type(string(4,*)), intent(in) :: c1(size(b1))

        pointer genChildArray(:)

        allocate (child(20,4) :: genChildArray(size(b1)))

        select type (genChildArray)
            type is (child(*,4))
!                genChildArray%base = genArray (b1)

                do i = 1, size(b1)
                    allocate (genChildArray(i)%data, source = b1(i))
                    if (allocated(c1(i)%str)) &
                        allocate (genChildArray(i)%str, source=c1(i)%str)
                end do

            class default
                stop 10
        end select
    end function
end module

program polyFuncRet002
use m
    class(base(:,4)), pointer :: b1(:,:)
    type(base(:,4)), allocatable :: b2(:,:)
    type(child(:,4)), allocatable :: c1(:,:)

    logical(4), external :: precision_r4

    b1(0:9, 0:19) => genArray([(i*1.0_4, i=1, 250)])

    b2 = b1

    b1 (-1:8, 1:20) => genArray([(i*1.0_4, i=1, 250)], &
        [(string(4,20)('test'//repeat(achar(mod(i, 128)),i)), i=1, 250)])

    select type (b1)
        type is (child(*,4))
            c1 = b1

        class default
            error stop 1_4
    end select

    !! verify b2 and c1
    if ((.not. allocated(b2)) .or. (.not. allocated(c1))) error stop 2_4

    if (any(lbound(b2) /= 0) .or. any(ubound(b2) /= [9, 19])) error stop 3_4

    if (any(lbound(c1) /= [-1,1]) .or. any(ubound(c1) /= [8,20])) error stop 4_4

    k = 1

    do j = 1, 20
        do i = 1, 10
            if (.not. precision_r4(b2(i-1,j-1)%data, k*1.0_4)) error stop 5_4

            if (.not. precision_r4(c1(i-2,j)%data, k*1.0_4)) error stop 6_4

            if (c1(i-2,j)%str /= 'test' // repeat(achar(mod(k, 128)),k)) &
                error stop 7_4
            k = k + 1
        end do
    end do
end
