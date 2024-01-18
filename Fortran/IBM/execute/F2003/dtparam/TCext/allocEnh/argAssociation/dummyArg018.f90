! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/argAssociation/dummyArg018.f
! opt variations: -qnol -qnodeferredlp

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 11/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that an allocatable dummy-arg is assigned
!                               to a dummy-procedure call.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)         x,y
    end type

    abstract interface
        function unitPoint (p)
            import
            class(point(*,4)), intent(in) :: p(:)

            type(point(20,4)) unitPoint(size(p))
        end function
    end interface

    contains

    subroutine assgnPoint (proc, p1, p2)
        procedure(unitPoint) :: proc
        type(point(:,4)), allocatable :: p1(:)

        type(point(*,4)), optional :: p2(:)

        if (allocated(p1)) then
            p1 = proc(p1)
        else
            if (present(p2)) p1 = proc(p2)
        end if
    end subroutine
end module

program dummyArg018
use m
    procedure(unitPoint) genPoint, selectConvert

    type(point(:,4)), allocatable :: p1(:)

    logical(4), external :: precision_r4

    call assgnPoint (genPoint, p1, [(point(20,4)(i, i+1), i=1, 10)])

    if (.not. allocated(p1)) error stop 1_4

    if ((lbound(p1,1) /= 1) .or. (ubound(p1,1) /= 10)) error stop 2_4

    do i = 1, 10
        if (.not. precision_r4(p1(i)%x, i/sqrt(1.0*(i**2+(i+1)**2)))) &
            error stop 3_4

        if (.not. precision_r4(p1(i)%y, (i+1)/sqrt(1.0*(i**2+(i+1)**2)))) &
            error stop 4_4
    end do

    call assgnPoint (selectConvert, p1)

    if ((lbound(p1,1) /= 1) .or. (ubound(p1,1) /= 10)) error stop 12_4

    do i = 1, 6
        if (.not. precision_r4(p1(i)%x, i*10/sqrt(1.0*(i**2+(i+1)**2)))) &
            error stop 5_4

        if (.not. precision_r4(p1(i)%y, (i+1)*10/sqrt(1.0*(i**2+(i+1)**2)))) &
            error stop 6_4
    end do

    do i = 7, 10
        if (.not. precision_r4(p1(i)%x, i/sqrt(1.0*(i**2+(i+1)**2)))) &
            error stop 7_4

        if (.not. precision_r4(p1(i)%y, (i+1)/sqrt(1.0*(i**2+(i+1)**2)))) &
            error stop 8_4
    end do
end


function genPoint (p)
use m
    class(point(*,4)), intent(in) :: p(:)

    type(point(20,4)) genPoint(size(p))

    real radius(size(p))

    forall (i = 1:size(p))
        radius(i) = sqrt(p(i)%x**2 + p(i)%y**2)

        genPoint(i)%x = p(i)%x/radius(i)
        genPoint(i)%y = p(i)%y/radius(i)
    end forall
end function


function selectConvert (p)
use m
    class(point(*,4)), intent(in) :: p(:)

    type(point(20,4)) selectConvert (size(p))

    real radius(size(p))

    where (p%y - p%x >= 1.0e-1)
        radius = sqrt(p%x**2+p%y**2)

        selectConvert%x = p%x*10/radius
        selectConvert%y = p%y*10/radius
    elsewhere
        selectConvert = p
    endwhere
end function
