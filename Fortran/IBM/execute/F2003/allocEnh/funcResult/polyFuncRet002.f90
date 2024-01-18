!#######################################################################
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
    type base
        real, pointer :: data => null()
    end type

    type, extends(base) :: child
        character(:), pointer :: str => null()
    end type

    type string
        character(:), allocatable :: str
    end type

    interface genArray
        procedure genBaseArray
        procedure genChildArray
    end interface

    contains

    class (base) function genBaseArray (b1)
        real, intent(in) :: b1(:)

        pointer genBaseArray(:)

        allocate (genBaseArray(size(b1)))

        do i = 1, size(b1)
            allocate (genBaseArray(i)%data, source=b1(i))
        end do
    end function


    class(base) function genChildArray (b1, c1)
        real, intent(in) :: b1(:)
        type(string), intent(in) :: c1(size(b1))

        pointer genChildArray(:)

        allocate (child :: genChildArray(size(b1)))

        select type (genChildArray)
            type is (child)
                genChildArray%base = genArray (b1)

                do i = 1, size(b1)
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
    class(base), pointer :: b1(:,:)
    type(base), allocatable :: b2(:,:)
    type(child), allocatable :: c1(:,:)

    logical(4), external :: precision_r4

    b1(0:9, 0:19) => genArray([(i*1.0_4, i=1, 250)])

    b2 = b1

    b1 (-1:8, 1:20) => genArray([(i*1.0_4, i=1, 250)], &
        [(string('test'//repeat(achar(mod(i, 128)),i)), i=1, 250)])

    select type (b1)
        type is (child)
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
