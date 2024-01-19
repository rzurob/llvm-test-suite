! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (errmsg in deallocate statement)
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

program falloc004a1
    class (*), allocatable :: x(:,:)

    integer stat1
    character(200) err

    stat1 = -1
    err = 'no error'

    deallocate (x, stat=stat1, errmsg=err)

    if ((stat1 /= 2) .or. (err == 'no error')) then
        print *, stat1, err
        error stop 1_4
    end if
end
