! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (deallocate of disassociated pointers
!                               will result in error conditions)
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

program falloc504
    class (*), pointer :: x, x1(:), x2(:,:)
    integer error

    nullify (x, x2)
    x1 => null()

    error = 0

    deallocate (x, stat=error)

    if (error /= 2) error stop 1_4

    error = 0

    deallocate (x1, stat=error)

    if (error /= 2) error stop 2_4

    error = 0

    deallocate (x2, stat=error)

    if (error /= 2) error stop 3_4
end
