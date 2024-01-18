! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/misc/fmisc022.f
! opt variations: -qnok -ql

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc022.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items
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

program fmisc022
    type base(k1)    ! (4)
        integer, kind :: k1
        class(*), pointer :: data(:)    !<-- class(*) is not of importance
    end type

    type (base(4)) :: b1

    data b1%data /null()/

    if (associated (b1%data)) error stop 1_4
    end
