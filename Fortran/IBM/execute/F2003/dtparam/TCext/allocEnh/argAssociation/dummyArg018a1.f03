! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/argAssociation/dummyArg018a1.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/13/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that an allocatable dummy-arg is assigned
!                               to a dummy-procedure call.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(k1)    ! (4)
        integer, kind :: k1
        real(k1)         x,y
    end type

    abstract interface
        elemental function unitPoint (p)
            import
            class(point(4)), intent(in) :: p

            type(point(4)) unitPoint
        end function
    end interface

    procedure(unitPoint) genPoint, renormalTo10

    contains

    subroutine assgnPoint (p1, p2, i)
        type(point(4)), allocatable :: p1(:)

        type(point(4)), optional :: p2(:)

        if (allocated(p1)) then
            if (i == 1) then
                p1 = genPoint (p1)
            else
                p1 = renormalTo10(p1)
            end if
        else
            if (present(p2)) then
                if (i == 1) then
                    p1 = genPoint (p2)
                else
                    p1 = renormalTo10 (p2)
                end if
            end if
        end if
    end subroutine
end module

program dummyArg018a1
use m
    type(point(4)), allocatable :: p1(:)

    logical(4), external :: precision_r4

    call assgnPoint (p1, [(point(4)(x=i, y=i+1), i=1,10)], 1)

    if (.not. allocated(p1)) error stop 1_4

    if ((lbound(p1,1) /= 1) .or. (ubound(p1,1) /= 10)) error stop 2_4

    do i = 1, 10
        if (.not. precision_r4(p1(i)%x, i/sqrt((i**2+(i+1)**2)*1.0))) &
            error stop 3_4

        if (.not. precision_r4(p1(i)%y, (i+1)/sqrt((i**2+(i+1)**2)*1.0))) &
            error stop 4_4
    end do

    call assgnPoint (p1, i = 10)

    do i = 1, 10
        if (.not. precision_r4(p1(i)%x, i*10/sqrt((i**2+(i+1)**2)*1.0))) &
            error stop 5_4

        if (.not. precision_r4(p1(i)%y, (i+1)*10/sqrt((i**2+(i+1)**2)*1.0))) &
            error stop 6_4
    end do
end


elemental function genPoint (p)
use m, only: point
    class(point(4)), intent(in) :: p

    type(point(4)) genPoint

    real radius

    radius = sqrt(p%x**2 + p%y**2)

    genPoint%x = p%x/radius
    genPoint%y = p%y/radius
end function


elemental function renormalTo10 (p)
use m, only: point
    class(point(4)), intent(in) :: p

    type(point(4)) renormalTo10

    real radius

    radius = sqrt(p%x**2 + p%y**2)

    renormalTo10%x = p%x/radius*10
    renormalTo10%y = p%y/radius*10
end function
