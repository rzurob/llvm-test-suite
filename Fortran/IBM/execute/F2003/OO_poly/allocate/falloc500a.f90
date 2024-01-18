!#######################################################################
!*  ===================================================================
!*
!*  DATE                       : 04/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE/DEALLOCATE (if the allocate-obj is not
!                                deallocated successfully, it will retain its
!                                previous allocation status)
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

program  falloc500a
    class(*), allocatable :: i

    integer err

    deallocate (i, stat = err)

    if (err /= 2) error stop 1_4

    if (allocated (i)) error stop 2_4
end
