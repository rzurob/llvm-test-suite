! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (run-time array bounds checking using
!                               -C; NOTE the execution is intentionally to fail)
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

program falloc501d1
    integer*4 :: size1, init1(3)
    integer*4, allocatable :: i1(:)

    init1 = (/1,2,3/)
    size1 = 5

    allocate (i1 (size1), source=init1)

end
