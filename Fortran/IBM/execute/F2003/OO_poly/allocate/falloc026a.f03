! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (deallocate a disassociated pointer
!                               will result in an error condition)
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

program falloc026
    class(*), pointer :: d1(:) => null()
    real(8), target :: d2(10)

    integer error

    deallocate (d1, stat=error)

    if (error == 0) error stop 1_4

    allocate (d1(10), source=(1.0, 2.0))

    error = 0

    deallocate (d1)
    deallocate (d1, stat=error)

    if (error == 0) error stop 2_4

    d1 => d2

    error = 0
    nullify (d1)

    deallocate (d1, stat=error)

    if (error == 0) error stop 3_4
end