!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr034.f
! %VERIFY: fconstr034.out:fconstr034.vf
! %STDIN:
! %STDOUT: fconstr034.out
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
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable scalar component; use derived
!                               type for the data-source)
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
    type container
        class(*), allocatable :: data
    end type
end module

module n
    type base
        integer(8) :: id
    end type

    type, extends(base) :: child
        character(18) :: name
    end type
end module

program fconstr034
use n
use m
    class(*), allocatable :: x1
    class (*), pointer :: x2(:)

    type (child), target :: c1 (2:10)

    allocate (x1, source=child(100, 'x1'))

    c1%id = (/(i,i=2,10)/)
    c1%name = 'c1_array_of_9'

    x2 => c1

    associate (y1 => container(x1), y2 => container(x2(5)))
        select type (z => y1%data)
            type is (child)
                print *, 'child type:', z
            type is (base)
                print *, 'base type:', z
            class default
                print *, 'wrong'
        end select

        select type (z => y2%data)
            class is (base)
                print *, 'class base'
            type is (child)
                print *, 'child:', z
            class default
                print *, 'wrong'
        end select
    end associate
end
