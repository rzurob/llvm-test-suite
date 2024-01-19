! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE/DEALLOCATE (deallocate of a pointer
!                               associated with a target that is not created by
!                               ALLOCATE statement will result in an error
!                               condition)
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

program falloc504a1
    integer(4), pointer :: i1(:), i2(:,:)
    class (*), pointer :: x, x1(:)

    integer error

    allocate (i1(10), i2 (2, 2))

    x => i1 (1)

    x1 => i2 (1,:)

    error = 0

    deallocate (x, stat=error)

    if (error /= 2) error stop 1_4

    error = 0
    deallocate (x1, stat=error)

    if (error /= 2) error stop 2_4

    x => i2 (1,1)

    error = 0
    deallocate (x, stat=error)

    if (error /= 2) error stop 3_4
end
