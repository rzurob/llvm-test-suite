!*  ===================================================================
!*
!*  DATE                       : 12/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLCOATE (failure in allocate will terminate
!                               execution if stat= not supplied)
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

program falloc022
    integer, allocatable :: x

    character(20) error

    deallocate (x, errmsg=error)

    print *, 'end'      !<-- this should never be executed
end
