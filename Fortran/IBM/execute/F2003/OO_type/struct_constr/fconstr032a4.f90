!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr032a4.f
! %VERIFY: fconstr032a4.out:fconstr032a4.vf
! %STDIN:
! %STDOUT: fconstr032a4.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (poly-pointers as the
!                               data-source for poly-allocatable component in
!                               the structure constructor)
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
    type dataType
        contains

        procedure :: print => printData
    end type

    type container
        integer(4) id
        class (dataType), allocatable :: data
    end type

    contains

    subroutine printData (d)
        class (dataType), intent(in) :: d

        print *, 'dataType'
    end subroutine
end module

module m1
use m, only : dataType
    type, extends(dataType) :: realData
        real(8) :: data

        contains

        procedure, pass(d) :: print => printRealData
    end type

    contains

    subroutine printRealData (d)
        class(realData), intent(in) :: d

        write (*, '(g10.3)') d%data
    end subroutine
end module

program fconstr032a4
use m
use m1
    class (dataType), pointer :: d1, d2(:)

    type (realData), target :: r1, r2(-1:0)
    type (container) :: co1

    d1 => r1
    d2 => r2

    r1%data = 1.0
    r2%data = (/-.5, .5/)

    associate (x => container(10, d1))
        if (.not. allocated (x%data)) error stop 1_4

        if (x%id /= 10) error stop 2_4
        call x%data%print
    end associate

    associate (x => container(20, d2(0)))
        if (x%id /= 20) error stop 3_4

        if (.not. allocated (x%data)) error stop 4_4

        call x%data%print
    end associate
end
