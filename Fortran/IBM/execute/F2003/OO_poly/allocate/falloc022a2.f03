! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/27/2005
!*
!*  DESCRIPTION                : allocate (deallocate will terminate the
!                               execution if failure occurs and stat= not
!                               present)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc022a2
    integer(4), pointer :: x

    allocate (x, x)

    print *, 'begin'

    deallocate (x, x)

    print *, 'end'
end