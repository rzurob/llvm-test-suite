! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/16/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 321657)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(:), allocatable :: c1

    character(200) c3

    c3 = 'xlftest'

    allocate(c1, source=trim(c3))

    if (c1%len /= 7) error stop 1_4

    if (c1 /= 'xlftest') error stop 2_4

    end