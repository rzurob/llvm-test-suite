! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (each allocate object that was not
!*                              successfully allocated shall retain its previous
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

program falloc013a1
    integer(4), parameter :: maxInt = 2**9 - 1

    integer(8), pointer :: x(:,:,:)

    integer(8), target :: y (2,3,4)

    integer error


    x => y

    if (.not. associated (x, y)) error stop 1_4
    error = 0

    !! the required size of the allocate statement exceeds the ceiling set for
    !this program
    allocate (x(maxInt,maxInt,maxInt), source=100_8, stat=error)

    if (error /= 1) error stop 2_4

    if (.not. associated (x, y)) error stop 3_4
end
