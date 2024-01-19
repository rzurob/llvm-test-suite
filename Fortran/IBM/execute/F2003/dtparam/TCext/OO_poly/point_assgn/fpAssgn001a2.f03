! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn001a2.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (association status
!*                               after pointer assignments; test associated())
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
    end type

    type, extends (base) :: child    ! (4)
        integer(k1) :: id
    end type
end module

program fpAssgn001a2
use m

    class (base(4)), pointer :: x => null()
    class (child(4)), pointer :: y => null()

    class (base(4)), pointer :: x1(:)
    class (child(4)), pointer :: y1(:)

    type (child(4)), target :: c1 = child(4) (id = 1)
    type (child(4)), target :: c2(2:100)

    x1 => null()
    y1 => null()

    x => y
    x1 => y1

    if (associated (x) .or. associated (x1) .or. associated(y) .or. &
        associated (y1)) error stop 1_4

    y => c1

    x => y

    if ((.not. associated (x, c1)) .or. (.not. associated (y, c1))) error stop 2_4

    c2 = (/(child(4)(i), i=2,100)/)

    y1 => c2

    x1 => y1
    if ((.not. associated(x1, c2)) .or. (.not. associated(y1, c2))) error stop 3_4

    x => x1(10)

    if ((.not. associated (x, y1(10))) .or. &
        (.not. associated (x, c2(10)))) error stop 4_4

    x1 => y1(2:3:2)

    if ((lbound(x1, 1) /= 1) .or. (ubound(x1,1) /= 1) .or. &
        (size (x1) /= 1)) error stop 5_4

    if ((lbound(y1, 1) /= 2) .or. (ubound(y1,1) /= 100) .or. &
        (size (y1) /= 99)) error stop 6_4

    if ((.not. associated(x1,c2(2:3:4))) .or. (.not. associated(x1,y1(2:2)))) &
            error stop 7_4
end
