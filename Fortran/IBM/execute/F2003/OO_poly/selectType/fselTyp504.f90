!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fselTyp504.f
! %VERIFY: fselTyp504.out:fselTyp504.vf
! %STDIN:
! %STDOUT: fselTyp504.out
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
!*  DESCRIPTION                : select type ((expr) as the selector)
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

program fselTyp504
    class(*), allocatable :: x1(:)

    allocate (x1(3), source=(/1_8,2_8,3_8/))

    select type (y => (x1))
        type is (integer(8))
            print *, cshift(y, 1)
    end select
end

