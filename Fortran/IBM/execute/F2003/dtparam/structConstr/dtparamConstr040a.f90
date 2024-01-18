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
!*  DATE                       : 03/21/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: One procedure used for two procedure
!                               pointer components in the same derived type;
!                               both with PASS attribute.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k)
        integer, kind :: k

        real(k) :: x, y

        procedure(movePoint), pointer :: move4
        procedure(movePoint), pass(p2), pointer :: move8
    end type

    contains

    subroutine movePoint (p1, p2, howmuch)
        class(point(4)), intent(inout) :: p1
        class(point(8)), intent(inout) :: p2
        class(point(4)), intent(in) :: howmuch

        p1%x = p1%x + howmuch%x
        p1%y = p1%y + howmuch%y


        p2%x = p2%x + howmuch%x
        p2%y = p2%y + howmuch%y
    end subroutine
end module

program dtparamConstr040a
use m
    type(point(8)) :: p8_1
    type(point(4)) :: p4_1, p4_2

    logical(4), external :: precision_r4

    p8_1 = point(8)(1.0d0, 2.1d0, null(), move8=movePoint)

    p4_1 = point(4)(y=1.2e0, x=.7e0, move4=movePoint, move8=null())
    p4_2 = point(4)(x = 5.2, y=-1.2, move4=null(), move8=null())

    call p8_1%move8(p4_1, p4_2)

    call p4_1%move4(p8_1, p4_2)

    !! verify the results of p4_1, p8_1
    if (.not. precision_r4(p4_1%x, .7+5.2+5.2)) error stop 1_4

    if (.not. precision_r4(p4_1%y, -1.2)) error stop 2_4

    if (.not. precision_r4(real(p8_1%x,4), real(1.0d0+5.2+5.2, 4))) &
            error stop 3_4

    if (.not. precision_r4(real(p8_1%y,4), real(2.1d0-1.2-1.2, 4))) &
            error stop 4_4
end
