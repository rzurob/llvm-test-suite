!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : defect367740.f
!*
!*  PROGRAMMER                 : Xing Xue
!*  DATE                       : July 31, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : -qcaf=images
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95_r
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

