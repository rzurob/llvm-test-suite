!#######################################################################
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
! %POSTCMD: tcomp ftpbnd500d4.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (C458: if external proc
!                               used as type bound, an explicit interface is
!                               required)
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

program ftpbnd500d4
    type base
        integer*4, pointer :: i

        contains

        procedure, nopass :: print => printBase
        procedure, nopass :: count => countBase
    end type

    type (base) :: b1

    call b1%print
end


    subroutine printBase
        print *, 'base'
    end subroutine

    integer function countBase (i)
        countBase = i
    end function
