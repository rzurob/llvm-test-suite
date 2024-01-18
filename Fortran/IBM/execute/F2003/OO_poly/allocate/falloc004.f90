!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP:
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (errmsg)
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

program falloc004
    real(8), allocatable :: r1
    integer(4) error
    character(200) reason

    reason = 'begin'

    allocate (r1, stat=error, errmsg = reason)

    if ((error /= 0) .or. (reason /= 'begin')) then
        error stop 1_4
    end if

    allocate (r1, stat=error, errmsg=reason)

    if ((error /= 2) .or. (reason == 'begin')) then
        print *, error, reason
        error stop 2_4
    end if
end
