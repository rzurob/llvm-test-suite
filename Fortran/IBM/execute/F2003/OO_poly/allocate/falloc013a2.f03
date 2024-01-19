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

program falloc013a2
    class (*), allocatable :: x(:,:,:)

    integer(4), parameter :: maxInt = 2**9 - 1

    integer error

    error = 0

    !! the required size for this allocate statement is so large that it will
    !certain cause failure of allocate statement)
    allocate (real(16) :: x(maxInt,maxInt,maxInt), stat = error)

    if (error /= 1) error stop 1_4

    if (allocated(x)) error stop 2_4
end
