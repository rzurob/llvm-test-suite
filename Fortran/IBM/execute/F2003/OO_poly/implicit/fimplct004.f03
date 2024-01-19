! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT statement (unlimited poly-entities
!*                               implied by IMPLICIT statement)
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

program fimplct004
    implicit class(*) (x)

    pointer x2
    allocatable x1(:)

    integer*4, target :: i1

    x2 => i1

    if (.not. associated (x2, i1)) error stop 1_4

    if (allocated (x1)) error stop 2_4

    allocate (integer*4 :: x1(4))

    if (.not. allocated (x1)) error stop 3_4

    if (size(x1) /= 4) error stop 4_4
end
