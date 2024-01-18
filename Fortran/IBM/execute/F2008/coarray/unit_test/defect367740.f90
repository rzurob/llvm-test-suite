!*  ===================================================================
!*
!*  DATE                       : July 31, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : -qcaf=images
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case is from defct 367740.
!*                               When -qcaf=images=1 is specified,
!*                               the PGAS RT aborts.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    program defect367740
    implicit none
    integer, save :: mypi[*]

    mypi = 1
    end

