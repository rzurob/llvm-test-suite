! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2005
!*
!*  DESCRIPTION                : allocate (source= not allowed in deallocate
!                               statement)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program  falloc500d
    class(*), allocatable :: i

    integer err

    deallocate (i, stat = err, source=100)  !<-- syntax error

    if (err /= 2) error stop 1_4

    if (allocated (i)) error stop 2_4
end