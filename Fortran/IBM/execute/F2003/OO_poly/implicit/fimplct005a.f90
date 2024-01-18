!#######################################################################
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
    implicit type (base) (b), type(child) (c)

    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    dimension c1_m(2:3), b1_m(0:3)

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fimplct005a
use m
    interface
        subroutine printData (d)
        use m
            implicit class (base) (d)
            dimension d(:)
            intent(in) :: d
        end subroutine
    end interface

    b1_m = (/(base(i), i= 0,3)/)

    call printData (b1_m)

    c1_m = (/child (2, 'c1_m_2'), child (3, 'c1_m_2')/)

    call printData (c1_m)
end

subroutine printData (d)
use m
    implicit class (base) (d)
    dimension d(:)
    intent(in) :: d

    do i = 1, size(d)
        call d(i)%print
    end do
end subroutine

