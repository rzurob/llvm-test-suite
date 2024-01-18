!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr042.f
! %VERIFY: fconstr042.out:fconstr042.vf
! %STDIN:
! %STDOUT: fconstr042.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (in a data statement)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
        procedure, non_overridable :: getID
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    type (base) :: b1_m
    type (child) :: c1_m(10)

    DATA b1_m /base(10)/, (c1_m (i), i=1,10) &
        /5*child(10, 'c1_m'), 4*child(100, 'c1_m1'), child(1, 'c1_m2')/

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b
        print *, 'base:', b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b
        print *, 'child:', b%id, b%name
    end subroutine

    integer*4 function getID (b)
        class (base), intent(in) :: b

        getID = b%id
    end function
end module

program fconstr042
use m
    type (child) :: c1(6)

    data c1(::2) /child(1,'c1_1'), child(3,'c1_3'), child(5,'c1_5')/

    c1(2::2) = (/(child(i, 'c1'), i=2,6,2)/)

    !! validate b1_m and c1_m
    call b1_m%print
    if (b1_m%getID() /= 10) error stop 1_4

    do i = 1, 5
        call c1_m(i)%print

        if (c1_m(i)%getID() /= 10) error stop 2_4
    end do

    do i = 6, 9
        call c1_m(i)%print

        if (c1_m(i)%getID() /= 100) error stop 3_4
    end do

    if (c1_m(10)%getID() /= 1) error stop 4_4


    !! validate c1
    do i = 1, 6
        if (c1(i)%getID() /= i) error stop 5_4

        call c1(i)%print
    end do
end
