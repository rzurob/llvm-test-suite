! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_tpbnd/specific/ftpbnd504d1.f
! opt variations: -qnok -ql

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
    type base(k1)    ! (4)
        integer, kind :: k1
        contains

        procedure, nopass, private :: print => printBase    !<-- illegal
    end type

    end

    subroutine printBase ()

    end subroutine
