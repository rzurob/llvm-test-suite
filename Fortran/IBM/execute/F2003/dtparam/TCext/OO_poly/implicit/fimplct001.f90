! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/implicit/fimplct001.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fimplct001.f
! %VERIFY: fimplct001.out:fimplct001.vf
! %STDIN:
! %STDOUT: fimplct001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT (simpliest testcase for class keyword
!*                               use with implicit)
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
        integer(k1)      id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fimplct001
use m
    implicit class (base(4)) (b)

    pointer b1

    type (child(4,20)), target :: c1

    c1 = child(4,20) (10, name = 'c1_test')

    b1 => c1

    call b1%print
end
