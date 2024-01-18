!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (allocate fails with execution
!                               terminated without stat=)
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

program falloc022a
    class (*), pointer :: x(:,:,:,:,:)

    integer(4), parameter :: nSize = 16
    character(200) error

    do i = 1, 1000000
        !! this allocate statement very likely to fail
        allocate (x(-nSize:nSize, -nSize:nSize, -nSize:nSize, -nSize:nSize, &
                    -nSize:nSize), source=(1.0, 2.0), errmsg = error)
    end do
end
