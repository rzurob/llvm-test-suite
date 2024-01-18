! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (subsequent redefinition and
!                               undefinition of any entities in the bound
!                               expression do not affect the array bounds)
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

program falloc008
    class (*), allocatable :: x1(:)
    integer(4) :: l, u

    l = 100
    u = 102

    call createData (x1, (1.0d0, .5d2), l, u)

    if (lbound(x1, 1) /= 100) error stop 1_4
    if (ubound(x1, 1) /= 102) error stop 2_4

    deallocate (x1)

    l = -1
    u = 2

    call createData (x1, 'xlftest', l, u)

    if (lbound(x1, 1) /= -1) error stop 3_4
    if (ubound(x1, 1) /= 2) error stop 4_4

    deallocate (x1)

    contains

    subroutine createData (x, source, lb, ub)
        class (*), allocatable, intent(out) :: x(:)
        class (*), intent(in) :: source
        integer(4), intent(inout) :: lb, ub

        allocate (x(lb:ub), source=source)

        lb = 0
        ub = -1
    end subroutine
end
