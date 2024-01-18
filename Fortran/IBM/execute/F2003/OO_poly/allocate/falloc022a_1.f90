!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (allocate statement will terminate
!                               the execution if fails)
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

program falloc022a_1
    real(16), allocatable :: x1(:,:)

    integer size1

    size1 = 10

    allocate (x1(size1, size1), stat=i1, source=1.0q0)

    if (i1 /= 0) error stop 1_4

    allocate (x1(2,2), source=1.5q1)

    print *, 'end'
end
