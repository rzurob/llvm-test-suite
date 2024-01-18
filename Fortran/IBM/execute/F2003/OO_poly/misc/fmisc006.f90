!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc006.f
! %VERIFY: fmisc006.out:fmisc006.vf
! %STDIN:
! %STDOUT: fmisc006.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 283328)
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

module m
    type base
        integer*4 :: id
    end type

    contains

    function replicateBase (b)
        type (base), intent(in) :: b
        type (base), pointer :: replicateBase

        type (base), target, save :: temp

        temp%id = b%id

        replicateBase => temp
    end function

    function replicateBase1 (b)
        type (base), intent(in) :: b
        type (base), allocatable :: replicateBase1

        allocate (replicateBase1)

        replicateBase1%id = b%id
    end function
end module

use m
    type (base), save :: b1, b2

    b1%id = 10

    b2 = replicateBase (b1)

    print *, b2, replicateBase (b1)

    b2 = replicateBase1 (b1)

    print *, b2, replicateBase1 (b1)

    print *, 'end'
end

