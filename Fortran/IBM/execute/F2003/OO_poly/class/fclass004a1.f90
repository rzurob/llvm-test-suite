!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass004a1.f
! %VERIFY: fclass004a1.out:fclass004a1.vf
! %STDIN:
! %STDOUT: fclass004a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : class keyword (intrinsic assignment, RHS can be
!*                               poly-entities; also contains allocatable
!*                               component)
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
        integer*4 i1
        integer*4 i2

        contains

        procedure :: print => printDataType
    end type

    type, extends(dataType) :: mData
        integer*4 i3

        contains

        procedure :: print => printmData
    end type

    type base
        class (dataType), allocatable :: data
        integer*4 :: id = 0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*15 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (allocated (b%data)) then
            call b%data%print
            print *, 'id = ', b%id
        end if
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        call b%base%print
        print *, b%name
    end subroutine

    subroutine printDataType (d)
        class (dataType), intent(in) :: d

        print *, d%i1, d%i2
    end subroutine

    subroutine printMdata (d)
        class (mData), intent(in) :: d

        print *, d%i1, d%i2, d%i3
    end subroutine
end module

program fclass004a1
use m
    type (base) :: b1
    class (base), pointer :: b2

    type (child), target :: c1
    type (mData) :: md1

    md1 = mData (10, 11, 12)

    c1 = child (null(), 20, 'c1')

    allocate (c1%data, source = md1)

    b2 => c1

    b1 = b2


    call b1%print

    call b2%print
end
