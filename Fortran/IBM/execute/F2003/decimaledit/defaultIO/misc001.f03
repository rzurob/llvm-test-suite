! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2006
!*
!*  DESCRIPTION                : miscellaneous (zero-size character array of
!                               in an allocate statement)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(:), allocatable :: c(:)

    allocate (c(0), source=(/character(20) :: /))

    if ((len (c) /= 20) .or. (c%len /= 20)) error stop 1_4

    if (size(c) /= 0) error stop 2_4

    end