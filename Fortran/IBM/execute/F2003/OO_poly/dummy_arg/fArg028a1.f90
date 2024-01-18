!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg028a1.f
! %VERIFY: fArg028a1.out:fArg028a1.vf
! %STDIN:
! %STDOUT: fArg028a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argumenet association (use array element
!                               designator for sequence assoication)
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

    type, extends(base) :: child
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

    subroutine printData (b)
        class (base), intent(in) :: b(3)

        call b(1)%print
        call b(2)%print
        call b(3)%print
    end subroutine
end module

program fArg028a1
use m
    class (base), allocatable :: b1 (:,:)
    type (child) :: c1 (10)

    c1 = (/(child(i, 'c1_'//char(ichar('0')+i-1)), i=1,10)/)

    call printData (c1 (3))

    allocate (b1(2,5), source=reshape(c1,(/2,5/)))

    print *, 'second call'

    call printData (b1((/1,2/),(/3, 4/)))   ! same as printData (c1(5))
end

