!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd518a.f
! %VERIFY: ftpbnd518a.out:ftpbnd518a.vf
! %STDIN:
! %STDOUT: ftpbnd518a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (type bound as actual
!*                               argument, specification expression)
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
        integer*4 id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printData (b, lb, ub)
        integer, intent(in) :: lb, ub
        class (base), intent(in) :: b(lb:ub)

        do i = lb, ub
            call b(i)%print
        end do
    end subroutine
end module

module m1
use m, only : base, child
    type dataType
        class (base), pointer :: data (:) => null()

        contains

        procedure :: lbound => lowerBound
        procedure :: ubound => upperBound
    end type

    contains

    integer function lowerBound (d)
        class (dataType), intent(in) :: d

        lowerBound = lbound (d%data, 1)
    end function

    integer function upperBound (d)
        class (dataType), intent(in) :: d

        upperBound = ubound (d%data, 1)
    end function
end module

program ftpbnd518a
use m
use m1, only : dataType
    type (dataType) :: d1

    type (base), target :: b1 (-1:4)
    type (child), target :: c1 (3:5)

    b1 = (/(base (id = i), i=-1,4)/)

    c1 = (/(child(id=i, name='c1_'//char(ichar('0')+i)), i=3,5)/)

    d1 = dataType (b1)

    call printData (d1%data, d1%lbound(), d1%ubound())

    d1 = dataType (data = c1)

    call printData (d1%data, d1%lbound(), d1%ubound())
end
