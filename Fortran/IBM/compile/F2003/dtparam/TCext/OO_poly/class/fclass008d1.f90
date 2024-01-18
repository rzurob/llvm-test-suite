! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass008d1.f
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
! %POSTCMD: tcomp fclass008d1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (selector's restrictions will be
!                               applied to associate-name: IO and intrinsic
!                               assignment on poly-entity is not allowed)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 1
    end type
end module

program fclass008d1
use m
    class (base(4)), allocatable :: b1

    allocate (b1)

    associate (x => b1)
        print *, x%id, x    !<-- illegal for IO

        x = base(4) (10)       !<-- illegal for intrinsic assignment
    end associate
end
