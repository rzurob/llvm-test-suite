!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/17/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the dummy-arg of allocatable variable
!                               arrays used in the intrinsic assignment;
!                               intrinsic types (deferred char-length and
!                               complex(8)).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

!! module m is designed to test deferred character allocatable dummy-arg
module m
    contains

    character(:) function concat (s, s1)
        character(:), allocatable :: s
        character(*), intent(in) :: s1

        allocatable concat

        if (allocated(s)) then
            s = s // s1
        else
            s = s1
        end if

        concat = s
    end function

    subroutine concatArray (s, s1, s2)
        character(:), allocatable :: s(:)
        character(*), intent(in) :: s1(:), s2(:)

        s = [(repeat(' ', s1%len+s2%len+1), i=1, max(size(s1), size(s2)))]

        do i = 1, min(size(s1), size(s2))
            call pasteChars (s(i), s1(i), s2(i))
        end do

        if (size(s1) >= size(s2)) then
            do i = size(s2) + 1, size(s1)
                call pasteChars (s(i), s1(i), '')
            end do
        else
            do i = size(s1) + 1, size(s2)
                call pasteChars (s(i), '', s2(i))
            end do
        end if
    end subroutine
end module

!! module m1 is used to test real(8)/complex(8) as allocatable dummy-arg in intrinsic
!assignment
module m1
    contains

    complex(8) function genCmplx (d1, d2)
        double precision, allocatable :: d1, d2

        allocatable genCmplx

        if (allocated(d1)) then
            if (allocated(d2)) then
                genCmplx = cmplx (d1, d2, 8)
            else
                genCmplx = cmplx(d1, 0.0d0, 8)
            end if
        else
            if (allocated(d2)) then
                genCmplx = cmplx(0.0d0, d2,8)
            else
                genCmplx = cmplx(0.0d0, 0.0d0)
            end if
        end if
    end function

    subroutine addCmplx (cx, cx1, d1)
        complex(8), allocatable :: cx(:)
        complex(8) cx1(:)
        real(8) d1(:)

        real(8), allocatable :: realPart, imagPart

        if (size(d1) < size(cx1)) stop 10

        cx = cx1

        do i = 1, size(cx)
            imagPart = d1(i)

            cx(i) = cx(i) + genCmplx (realPart, imagPart)
        end do
    end subroutine
end module

program dummyArg005
use m
use m1
    character(:), allocatable :: string, strings(:)

    character(*), parameter :: s_const (0:9) &
        = ['0','1','2','3','4','5','6','7','8','9']

    complex(8), allocatable :: cx(:)
    complex(8) cx1 (100)

    logical(4), external :: precision_x6

    call concatArray (strings, [('test', 'TEST', i=1,5)], s_const)

    do i = 1, 5
        if (strings(2*i-1) /= 'test '//achar(46+i*2)) error stop 1_4
        if (strings(2*i) /= 'TEST ' // achar(47+i*2)) error stop 2_4
    end do

    !! intrinsic assign each element
    do i = 1, 10
        string = 'xlf '

        strings(i) = concat (string, s_const(i-1))

        if (string /= 'xlf ' // s_const(i-1)) error stop 3_4
    end do

    do i = 1, 10
        if (strings(i) /= 'xlf '//achar(47+i)) error stop 4_4
    end do

    cx1 = [(i, i=1,100)]

    call addCmplx (cx, cx1, [(i*1.0d0, i=1,100)])

    if (size(cx) /= 100) error stop 5_4

    do i = 1, 100
        if (.not. precision_x6 (cx(i), cmplx(i,i,8))) error stop 6_4
    end do
end


subroutine pasteChars (s, s1, s2)
    character(*) s, s1, s2

    s = s1 // ' ' // s2
end subroutine
