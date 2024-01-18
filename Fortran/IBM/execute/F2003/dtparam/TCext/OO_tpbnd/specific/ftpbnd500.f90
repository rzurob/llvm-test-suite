! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd500.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd500.f
! %VERIFY: ftpbnd500.out:ftpbnd500.vf
! %STDIN:
! %STDOUT: ftpbnd500.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific binding (inherited binding accessible
!*                               even if the base type is not)
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
    type, private :: base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: print => printBase
    end type

    type, extends(base) :: child    ! (4,20)
        integer(k1), pointer :: id => null()
    end type

    type (base(4,20)) :: b1_m
    type (child(4,20)), save :: c1_m

    contains

    subroutine printBase
        print *, 'base'
    end subroutine printBase

end module

program ftpbnd500
use m, only : child, c1_m, b1_m

    type (child(4,20)) :: c1
    class (child(4,:)), allocatable :: c2

    call c1%print

    allocate (child(4,20) :: c2)

    call c2%print

    call c1_m%print

    call b1_m%print
end
