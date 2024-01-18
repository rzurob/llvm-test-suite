! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc024.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (stat-variable can be of integer with
!                               different kind type parameters)
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
    end type
end module

program falloc024
use m
    integer error
    integer(1) err1
    integer*2 :: err2
    integer(8) :: err8 = 100

    class (base(4)), allocatable :: b1, b2(:)


    error = 10
    err1 = 100
    err2 = 1000
    allocate (b1, stat=err8)

    if (err8 /= 0) error stop 1_4

    allocate (b1, stat= err8)

    if (err8 /= 2) error stop 2_4

    allocate (b2(-1:2), stat = err1, source = base(4)(10))

    if (err1 /= 0) error stop 3_4

    allocate (b2(-1:2), stat = err1, source = base(4)(10))

    if (err1 /= 2) error stop 4_4

    allocate (b2(0:-1), stat=err2, source=base(4)(1))

    if (err2 /= 2) error stop 5_4

    deallocate (b2, stat= err2)

    if (err2 /= 0) error stop 6_4
end
