! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/misc/fmisc021a.f
! opt variations: -qnol

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc021a.f
! %VERIFY: fmisc021a.out:fmisc021a.vf
! %STDIN:
! %STDOUT: fmisc021a.out
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
!*  DESCRIPTION                : miscellanous items (defect 294510)
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

program fmisc021a
    type A(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i, j
    end type

    type (A(20,4)) a1(3)

    a1%i = (/1, 2, 3/)
    a1%j = (/10, 20, 30/)


    associate (x1 => cshift (a1, 1), x2 => eoshift (a1, 1, A(20,4)(0,0)))
        print *, x1
        print *, x2
    end associate
    end
