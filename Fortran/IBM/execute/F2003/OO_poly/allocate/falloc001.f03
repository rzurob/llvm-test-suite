! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (type-spec in allocate statement)
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

program falloc001
    class (*), pointer :: x1(:)

    class (*), allocatable :: x2

    allocate (complex(4):: x1 (2:5), x2)

    if ((.not. associated (x1)) .or. (.not. allocated (x2))) error stop 1_4

    if ((lbound (x1, 1) /= 2) .or. (ubound (x1, 1) /= 5)) error stop 2_4
end