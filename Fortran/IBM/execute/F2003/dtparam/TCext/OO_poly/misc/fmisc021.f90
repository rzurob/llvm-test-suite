! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/misc/fmisc021.f
! opt variations: -ql

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc021.f
! %VERIFY: fmisc021.out:fmisc021.vf
! %STDIN:
! %STDOUT: fmisc021.out
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
!*  DESCRIPTION                : miscellaneous items (defect 294510)
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

program fmisc021
    type A(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i, j
    end type

    type (A(4)) a1(3)

    a1%i = (/1, 2, 3/)
    a1%j = (/10, 20, 30/)

    print *, cshift (a1, 1)
    print *, eoshift (a1, 1, A(4)(0,0))

end
