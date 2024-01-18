!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn017.f
! %VERIFY: fpAssgn017.out:fpAssgn017.vf
! %STDIN:
! %STDOUT: fpAssgn017.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (structure component
!*                               used to contain different types)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    private printBase, printChild

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'Base: id =', b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, 'Child: id =', b%id, 'name =', b%name
    end subroutine
end module

module m1
use m
    type dataType
        private
        class (base), pointer :: data => null()

        contains

        procedure :: assgn => AssgnData
        procedure :: print => printDataType
    end type

    type (dataType), save :: d1 (10)

    private AssgnData, printDataType

    contains

    subroutine printDataType (d)
        class (dataType), intent(in) :: d

        call d%data%print
    end subroutine

    subroutine AssgnData (d, d1)
        class (dataType), intent(inout) :: d
        class (base), intent(in), target :: d1

        d%data => d1
    end subroutine
end module

program fpAssgn017
use m1

    type (child), target :: c1(5)
    type (base), target :: b1(5)

    b1 = (/(base(id = i), i=1,5)/)

    c1 = (/(child (id = i, name = 'c1'), i=6,10)/)

    do i = 1, 5
        call d1(2*i-1)%assgn(b1(i))
        call d1(2*i)%assgn(c1(i))
    end do

    do i = 1, 10
        call d1(i)%print
    end do
end
