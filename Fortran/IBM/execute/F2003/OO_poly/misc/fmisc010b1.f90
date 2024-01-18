!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc010b1.f
! %VERIFY: fmisc010b1.out:fmisc010b1.vf
! %STDIN:
! %STDOUT: fmisc010b1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 277245; problem #3:
!                               structure constructor with allocatable component
!                               used in an implied-do in an array constructor)
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
        integer(4) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15) :: name

        contains

        procedure :: print => printChild
    end type

    type dataType
        class (base), allocatable :: data

        contains

        procedure :: print => printDataType
    end type

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printDataType (d)
        class (dataType), intent(in) :: d

        call d%data%print
    end subroutine
end module

program fmisc010b1
use m
    type (dataType) :: d1 (10)

    d1 = (/(dataType(child(i, 'temp')), i = 1, 10)/)

    ! verify the results
    do i = 1, 10
        call d1(i)%print
    end do
end
