! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/27/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 323271)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(5), allocatable :: mode

    mode = 'COMMA'

    if (.not. allocated(mode)) error stop 1_4

    if (mode /= 'COMMA') error stop 2_4
    end
