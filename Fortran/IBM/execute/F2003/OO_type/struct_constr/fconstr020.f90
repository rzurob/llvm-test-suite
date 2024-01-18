!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr020.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
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
    type point
        real*4 :: x, y
    end type
end module

program fconstr020
use m

    type lineSegment
        type(point) :: a, b
    end type

    type (point) :: p1 = point (0.0, 0.0)
    type (point) :: p2 = point (1.0, 1.0)

    type (lineSegment) :: l1 = lineSegment &
            (b = point (1.0, 1.0), a = point (0.0, 0.0))

    type (lineSegment) :: l2 = lineSegment &
            (point(0.0, 0.0), point (1.0, 0.0))

    type (lineSegment) :: l3, l4

    l3 = lineSegment (b = p1, a = p2)
    l4 = lineSegment (p1, p2)

    ! validate the lineSegments
    if (.not. validateLine(l1, p1, p2)) error stop 1_4

    if (.not. validateLine(l2, point(0.0, 0.0), point (1.0, 0.0))) &
                            error stop 2_4

    if (.not. validateLine(l3, point (1.0, 1.0), p1)) error stop 3_4

    if (.not. validateLine(l4, p1, point (1.0, 1.0))) error stop 4_4

    l4 = lineSegment (p1, b = p2)

    if (.not. validateLine(l4, p1, p2)) error stop 5_4

    l4 = lineSegment (point(0.0, 0.0), b = p1)  ! this is not a line

    if (validateLine(l4, p1, p1)) error stop 6_4

    contains
        logical function isSamePoint (pa, pb)
            type (point), intent(in) :: pa, pb

            isSamePoint = ((pa%x == pb%x) .and. (pa%y == pb%y))
        end function

        logical function validateLine (l, pa, pb)
            type (lineSegment), intent(in) :: l
            type (point), intent(in) :: pa, pb

            validateLine = (isSamePoint (l%a, pa) .and. isSamePoint (l%b, pb))
            validateLine = (validateLine .and. (.not. isSamePoint(l%a, l%b)))
        end function
end
