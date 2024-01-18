! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (each allocate object that was not
!                               successfully allocated shall retain its previous
!                               allocation status or pointer association status)
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

program falloc013
    class (*), allocatable :: x1(:)
    integer error

    error = 0

    allocate (x1(2), source=(/1.0d0, .5d0/))

    allocate (integer(8) :: x1(-1:1), stat=error)

    if (error /= 2) error stop 1_4

    if (.not. allocated (x1)) error stop 2_4

    if (lbound(x1, 1) /= 1) error stop 3_4
    if (ubound(x1, 1) /= 2) error stop 4_4
end
