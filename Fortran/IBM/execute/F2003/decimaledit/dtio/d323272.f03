! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/24/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 323272)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(400), allocatable :: c

    allocate (c, source=repeat(' ', 400))

    if ((.not. allocated(c)) .or. (len(c) /= 400)) error stop 1_4

    if (c /= '') error stop 2_4
    end