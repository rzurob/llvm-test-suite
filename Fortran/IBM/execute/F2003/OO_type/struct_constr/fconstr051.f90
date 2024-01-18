!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr051.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (poly-entities used as
!                               the data-source for nonpoly scalar component)
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
    end type

    type, extends(base) :: child
        character(15) :: name
    end type
end module

program fconstr051
use m
    type container
        type (base) :: b1
    end type

    class (base), allocatable :: b1
    class (base), pointer :: b2(:)

    type (child), target :: c1(2:10)

    b2 => c1
    allocate (b1, source=child(1,'b1'))

    c1%id = (/(i, i=2,10)/)
    c1%name = 'c1_array_of_9'

    associate (x1 => container (b1), x2 => container (b2(3)))
        if ((x1%b1%id /= 1) .or. (x2%b1%id /= 3)) error stop 1_4
    end associate
end
