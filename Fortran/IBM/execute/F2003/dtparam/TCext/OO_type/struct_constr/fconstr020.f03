! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr020.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (struct_constr in
!*                               struct_constr; containment relationship)
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
    type point(k1)    ! (4)
        integer, kind :: k1
        real(k1)      :: x, y
    end type
end module

program fconstr020
use m

    type lineSegment(k2,n1)    ! (4,20)
        integer, kind   :: k2
        integer, len    :: n1
        type(point(k2)) :: a, b
    end type

    type (point(4)) :: p1 = point(4) (0.0, 0.0)
    type (point(4)) :: p2 = point(4) (1.0, 1.0)

    type (lineSegment(4,20)) :: l1 = lineSegment(4,20) &
            (b = point(4) (1.0, 1.0), a = point(4) (0.0, 0.0))

    type (lineSegment(4,20)) :: l2 = lineSegment(4,20) &
            (point(4)(0.0, 0.0), point(4) (1.0, 0.0))

    type (lineSegment(4,20)) :: l3, l4

    l3 = lineSegment(4,20) (b = p1, a = p2)
    l4 = lineSegment(4,20) (p1, p2)

    ! validate the lineSegments
    if (.not. validateLine(l1, p1, p2)) error stop 1_4

    if (.not. validateLine(l2, point(4)(0.0, 0.0), point(4) (1.0, 0.0))) &
                            error stop 2_4

    if (.not. validateLine(l3, point(4) (1.0, 1.0), p1)) error stop 3_4

    if (.not. validateLine(l4, p1, point(4) (1.0, 1.0))) error stop 4_4

    l4 = lineSegment(4,20) (p1, b = p2)

    if (.not. validateLine(l4, p1, p2)) error stop 5_4

    l4 = lineSegment(4,20) (point(4)(0.0, 0.0), b = p1)  ! this is not a line

    if (validateLine(l4, p1, p1)) error stop 6_4

    contains
        logical function isSamePoint (pa, pb)
            type (point(4)), intent(in) :: pa, pb

            isSamePoint = ((pa%x == pb%x) .and. (pa%y == pb%y))
        end function

        logical function validateLine (l, pa, pb)
            type (lineSegment(4,*)), intent(in) :: l
            type (point(4)), intent(in) :: pa, pb

            validateLine = (isSamePoint (l%a, pa) .and. isSamePoint (l%b, pb))
            validateLine = (validateLine .and. (.not. isSamePoint(l%a, l%b)))
        end function
end
