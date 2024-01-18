!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod && export XLFRTEOPTS="iostat_end=2003std"
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc027.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: unset XLFRTEOPTS
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/05/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellenaous items (defect 297674)
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

program fmisc027
    use iso_fortran_env
    character(3) c1
    character (4) c2, c3

    c1 = 'abc'

    read (c1, *, iostat=i1) c2, c3

    if (i1 /= iostat_end) error stop 1_4
end
