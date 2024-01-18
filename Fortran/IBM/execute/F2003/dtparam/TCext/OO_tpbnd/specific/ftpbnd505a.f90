! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd505a.f
! opt variations: -qnol

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd505a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (elemental PASS function
!*                               binding; basic comparason)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure :: equal => baseEqual
    end type

    contains

    elemental logical*4 function baseEqual (b, b1)
        class (base(*,4)), intent(in) :: b, b1

        baseEqual = (b%id == b1%id)
    end function

end module

program ftpbnd505a
use m
    type (base(20,4)) :: b1, b2, b3(10), b4(10)
    logical*4 :: res(10)

    b1 = base(20,4)(10)
    b2 = b1

    b3 = (/(base(20,4)(id = i), i=3,12)/)

    b4 = b3

    !! elemental binding invoked by scalar
    if (.not. b1%equal(b2)) error stop 1_4

    if (.not. b1%equal (b1)) error stop 2_4


    !! invoked for array elements
    do i = 1, 10
        if (.not. b3(i)%equal(b4(i))) error stop 3_4
    end do


    !! invoked by array on array
    res = b4%equal(b3)

    if (.not. all (res)) error stop 4_4

    if (.not. all (b3%equal(b4))) error stop 5_4


    !! invoked by array on scalar
    res = b3%equal (b4(2))      ! res(2) is true, false for others

    if ((.not. res(2)) .or. res(1) .or. any(res(3:))) error stop 6_4

    !! invoked by scalar on array
    res = b3(2)%equal (b4)      ! res(2) is true, false for others

    if ((.not. res(2)) .or. res(1) .or. any(res(3:))) error stop 7_4
end
