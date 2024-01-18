!*  ===================================================================
!*
!*  DATE                       : 01/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (errmsg used in deallocate statement)
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

program falloc004a
    class (*), pointer :: x(:)

    integer(8), target :: i1(10)
    class(*), target, allocatable :: x2(:)

    integer stat1
    character(200) error

    stat1 = -1
    error = 'no error'

    allocate (x2(3), source=1.0)

    !! this call should succeed
    deallocate (x2, stat=stat1, errmsg=error)

    if ((stat1 /= 0) .or. (error /= 'no error')) then
        print *, stat1, error
        error stop 1_4
    end if

    x => i1

    !! this call should fail
    deallocate (x, stat=stat1, errmsg=error)

    if ((stat1 /= 2) .or. (error == 'no error')) then
        print *, stat1, error
        error stop 2_4
    end if


    allocate (x2(3), source=1.0)

    x => x2

    stat1 = -1
    error = 'no error'

    !! this deallocate statement should fail
    deallocate (x, stat=stat1, errmsg=error)

    if ((stat1 /= 2) .or. (error == 'no error')) then
        print *, stat1, error
        error stop 3_4
    end if
end
