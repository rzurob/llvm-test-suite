! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2005
!*
!*  DESCRIPTION                : sepcific type bound (C455 and C458)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program ftpbnd504d1
    type base
        contains

        procedure, nopass, private :: print => printBase    !<-- illegal
    end type

    end

    subroutine printBase ()

    end subroutine