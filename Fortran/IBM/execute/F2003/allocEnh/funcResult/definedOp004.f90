! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/20/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test a defined operator for arrays of derived
!                               type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type string
        character(:), allocatable :: str
    end type

    interface operator (//)
        procedure concatStrings
    end interface

    contains

    function concatStrings (s1, s2)
        type(string), intent(in) :: s1(:), s2(:)

        type(string) concatStrings(max(size(s1), size(s2)))

        do i = 1, min(size(s1), size(s2))
            if (allocated(s1(i)%str)) then
                if (allocated(s2(i)%str)) then
                    concatStrings(i)%str = s1(i)%str // s2(i)%str
                else
                    concatStrings(i)%str = s1(i)%str
                end if
            else
                if (allocated(s2(i)%str)) concatStrings(i)%str = s2(i)%str
            end if
        end do

        if (size(s1) >= size(s2)) then
            do i = min(size(s1), size(s2))+1, size(s1)
                if (allocated(s1(i)%str)) concatStrings(i)%str = s1(i)%str
            end do
        else
            do i = min(size(s1), size(s2))+1, size(s2)
                if (allocated(s2(i)%str)) concatStrings(i)%str = s2(i)%str
            end do
        end if
    end function
end module

use m
    type(string), allocatable :: s1(:), s2(:), s3(:)

    s1 = (/(string(null()), string(genString(i)), i=1,100)/)

    s2 = (/(string(genString(i*2)), string(null()), string('XYZ'), i=1,20)/)

    ! test 1: two whole arrays
    s3 = s1 // s2

    do i = 1, 60, 6
        if (s3(i)%str /= genString(2*(i+2)/3)) error stop 1_4

        if (s3(i+1)%str /= genString((i+1)/2)) error stop 2_4

        if (s3(i+2)%str /= 'XYZ') error stop 3_4

        if (s3(i+3)%str /= genString((i+3)/2) // genString(2*(i+5)/3)) &
            error stop 4_4

        if (allocated(s3(i+4)%str)) error stop 5_4

        if (s3(i+5)%str /= genString((i+5)/2) // 'XYZ') error stop 6_4
    end do

    do i = 61, 200, 2
        if (allocated(s3(i)%str)) error stop 7_4

        if (s3(i+1)%str /= genString((i+1)/2)) error stop 8_4
    end do

    !! test 2
    s3 = s3(3:60:6) // s2(3::3)

    if (size(s3) /= 20) error stop 9_4

    do i = 1, 10
        if (s3(i)%str /= 'XYZXYZ') error stop 10_4
    end do

    do i = 11, 20
        if (s3(i)%str /= 'XYZ') error stop 11_4
    end do

    contains

    function genString (i)
        integer, intent(in) :: i
        character(i), allocatable :: genString

        genString = repeat(achar(65+mod(i,26)), i)
    end function
end
