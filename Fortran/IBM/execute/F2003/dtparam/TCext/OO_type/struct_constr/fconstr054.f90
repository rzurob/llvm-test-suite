! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_type/struct_constr/fconstr054.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2005
!*
!*  DESCRIPTION                : structure constructor (use the generic function
!                               to override the structure constructor; use the
!                               allocatable rank-two component)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind                     :: k1
        integer(k1), allocatable, private :: val(:,:)

        contains

        procedure :: print => printBase
        procedure :: sum => sumAllVal
        procedure :: max => maxValue
        procedure :: average => averageOfVal
    end type

    interface base
        module procedure createBase
    end interface

    contains

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        do j = lbound(b%val,2), ubound(b%val,2)
            do i = lbound(b%val,1), ubound(b%val,1)
                print *, b%val(i,j)
            end do
        end do
    end subroutine

    integer(8) function sumAllVal (b)
        class (base(8)), intent(in) :: b

        sumAllVal = sum (b%val)
    end function

    integer(8) function maxValue (b)
        class (base(8)), intent(in) :: b

        maxValue = maxVal (b%val)
    end function

    integer(8) function averageOfVal (b)
        class (base(8)), intent(in) :: b

        if (size(b%val) == 0) then
            averageOfVal = 0
        else
            averageOfVal =  b%sum() /size(b%val)
        end if
    end function

    type (base(8)) function createBase (i8, shape)
        integer(8), intent(in) :: i8(:)
        character(*), intent(in) :: shape

        integer lowerBound(2), upperBound(2)
        character (20) :: localBuf
        integer localIndex

        !!the string shape should be in a format like '2:3,1:5'
        localBuf = shape

        localIndex = index(localBuf, ':')
        localBuf (localIndex:localIndex) = ','

        localIndex = index(localBuf, ':')
        localBuf (localIndex:localIndex) = ','

        read (localBuf, *) lowerBound(1), upperBound(1), lowerBound(2), &
                        upperBound(2)

        allocate (createBase%val(lowerBound(1):upperBound(1), &
                    lowerBound(2):upperBound(2)), source=reshape (i8, &
                    (/upperBound(1)-lowerBound(1)+1, &
                    upperBound(2)-lowerBound(2)+1/)))
    end function
end module

program fconstr054
use m
    type (base(8)) :: b1
    integer(8) i
    integer(8), allocatable :: i2(:)

    !! 1st test
    b1 = base((/(i, i=1_8,20_8,2_8)/), '2:3, 2:6')

    if (b1%sum() /= 100_8) error stop 1_4
    if (b1%max() /= 19_8) error stop 2_4
    if (b1%average() /= 10_8) error stop 3_4

    call b1%print()

    !! 2nd test
    print *, new_line('a'), ' second test', new_line('a')
    allocate (i2(6), source=(/2_8, 10_8, 3_8, 1_8, 1_8, 5_8/))

    associate (x => base(i2, '1:3, 0:1'))
        if (x%max() /= 10) error stop 4_4
        if (x%average() /= 3) error stop 5_4
        if (x%sum() /= 22) error stop 6_4

        call x%print
    end associate
end
