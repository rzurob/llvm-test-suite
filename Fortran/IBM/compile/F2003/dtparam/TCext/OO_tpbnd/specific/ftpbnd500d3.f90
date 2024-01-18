! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd500d3.f
! opt variations: -qnol

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp ftpbnd500d3.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (C458: internal procedure
!                               can NOT type bound proc)
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

program ftpbnd500d3
    type base(n1,k1)    ! (20,4)
        integer, kind        :: k1
        integer, len         :: n1
        integer(k1), pointer :: i

        contains

        procedure, nopass :: print => printBase
        procedure, nopass :: count => countBase
    end type

    type (base(20,4)) :: b1

    call b1%print

    contains

    subroutine printBase
        print *, 'base'
    end subroutine

    integer function countBase (i)
        countBase = i
    end function
end
