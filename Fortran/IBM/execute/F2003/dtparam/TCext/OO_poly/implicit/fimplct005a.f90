! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/implicit/fimplct005a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fimplct005a.f
! %VERIFY: fimplct005a.out:fimplct005a.vf
! %STDIN:
! %STDOUT: fimplct005a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT statement (implicit of poly-entities
!*                               used in external procedure)
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
    implicit type (base(4)) (b), type(child(4,20)) (c)

    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name

        contains

        procedure :: print => printChild
    end type

    dimension c1_m(2:3), b1_m(0:3)

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

program fimplct005a
use m
    interface
        subroutine printData (d)
        use m
            implicit class (base(4)) (d)
            dimension d(:)
            intent(in) :: d
        end subroutine
    end interface

    b1_m = (/(base(4)(i), i= 0,3)/)

    call printData (b1_m)

    c1_m = (/child(4,20) (2, 'c1_m_2'), child(4,20) (3, 'c1_m_2')/)

    call printData (c1_m)
end

subroutine printData (d)
use m
    implicit class (base(4)) (d)
    dimension d(:)
    intent(in) :: d

    do i = 1, size(d)
        call d(i)%print
    end do
end subroutine

