!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/27/2005
!*
!*  DESCRIPTION                : allocate (deallocate failure will terminate the
!                               execution if stat= not present)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class (*), pointer :: data(:)

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        if (associated (b%data)) then
            print *, 'deallocating data'

            deallocate (b%data)
        end if
    end subroutine
end module

program falloc022a3
use m
    class (base), allocatable :: b

    integer(8), target :: i1(2)
    integer stat1
    character(200) err

    allocate (b)

    b%data => i1

    !! this deallocate should fail at run time
    deallocate (b, stat=stat1, errmsg=err)
end
