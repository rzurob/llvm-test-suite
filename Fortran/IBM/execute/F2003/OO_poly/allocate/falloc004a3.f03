! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (errmsg in allocate statement that
!                               runs out of resources)
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

program falloc004a3
    class (*), pointer :: x(:)

    integer stat1
    character(200) err

    integer, parameter :: largeSize = 190000**3/4

    do i = 1, 100000
        err = 'no error'

        allocate (integer(4) :: x(largeSize), stat = stat1, errmsg=err)

        if (stat1 /= 0) exit
    end do

    if ((stat1 /= 1) .or. (err == 'no error')) error stop 1_4
end