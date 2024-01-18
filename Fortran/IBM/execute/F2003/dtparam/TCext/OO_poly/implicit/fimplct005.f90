! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/implicit/fimplct005.f
! opt variations: -qck -ql

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fimplct005.f
! %VERIFY: fimplct005.out:fimplct005.vf
! %STDIN:
! %STDOUT: fimplct005.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT (implicit used in type bound
!*                               declaration)
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
    implicit class (base(4)) (b)
        intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
    implicit class (child(4,*)) (b)
        intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fimplct005
use m
    implicit class (base(4)) (b), class (child(4,20)) (c)

    pointer b1, c1

    allocate (b1)
    allocate (c1)

    b1%id = 1

    c1%id = 10
    c1%name = 'c1'

    call b1%print
    call c1%print

    deallocate (b1)

    b1 => c1

    call b1%print

    deallocate (b1)
end
