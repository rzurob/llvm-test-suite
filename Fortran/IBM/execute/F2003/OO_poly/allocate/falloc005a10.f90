!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005a10.f
! %VERIFY: falloc005a10.out:falloc005a10.vf
! %STDIN:
! %STDOUT: falloc005a10.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (function reference as source-expr in
!                               ALLOCATE statement)
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
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    interface makeData
        function produceBaseObj (id)
            import base
            type (base) produceBaseObj
            integer(4), intent(in) :: id
        end function

        function produceChildObj (id, name)
            import child
            type (child) produceChildObj
            integer(4), intent(in) :: id
            character(*), intent(in) :: name
        end function
    end interface

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

program falloc005a10
use m
    class (base), allocatable :: b1, b2(:)

    type (child), pointer :: c1

    allocate (b1, source=makeData (10))

    allocate (b2(3:5), source=makeData (20, 'b2_array_of_3'))

    allocate (c1, source=makeData (30, 'c1'))

    call b1%print

    call c1%print

    call b2(3)%print
    call b2(4)%print
    call b2(5)%print
end

type (base) function produceBaseObj (id)
use m, only: base
    integer(4), intent(in) :: id

    produceBaseObj%id = id
end function


type (child) function produceChildObj (id, name)
use m, only: child
    integer(4), intent(in) :: id
    character(*), intent(in) :: name

    produceChildObj%id = id
    produceChildObj%name = name
end function
