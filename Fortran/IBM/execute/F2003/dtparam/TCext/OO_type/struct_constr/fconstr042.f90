! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr042.f
! SCCS ID Information
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
        procedure, non_overridable :: getID
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    type (base(4)) :: b1_m
    type (child(4,1,20)) :: c1_m(10)

    DATA b1_m /base(4)(10)/, (c1_m (i), i=1,10) &
        /5*child(4,1,20)(10, 'c1_m'), 4*child(4,1,20)(100, 'c1_m1'), child(4,1,20)(1, 'c1_m2')/

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b
        print *, 'base:', b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b
        print *, 'child:', b%id, b%name
    end subroutine

    integer*4 function getID (b)
        class (base(4)), intent(in) :: b

        getID = b%id
    end function
end module

program fconstr042
use m
    type (child(4,1,20)) :: c1(6)

    data c1(::2) /child(4,1,20)(1,'c1_1'), child(4,1,20)(3,'c1_3'), child(4,1,20)(5,'c1_5')/

    c1(2::2) = (/(child(4,1,20)(i, 'c1'), i=2,6,2)/)

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
