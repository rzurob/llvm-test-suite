! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (function return results
!                               as actual-arg)
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
        character*20 :: name ='default'

        contains

        procedure :: print => printChild
    end type

    interface makeData
        function makeBase (id, n)
        import base
            integer*4, intent(in) :: id, n
            type (base) :: makeBase(n)
        end function

        function makeChildData (id, name, n)
        import child
            integer*4, intent(in) :: id, n
            character(*), intent(in) :: name

            type (child) :: makeChildData (n)
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

    subroutine printData (b)
        class (base), intent(in) :: b(:)

        do i = 1, size (b)
            call b(i)%print
        end do
    end subroutine
end module

program fArg029
use m
    call printData (makeData (10, 3))

    call printData (makeData (20, 'temps', 2))
end

type (base) function makeBase (id, n)
use m, only: base
    integer*4, intent(in) :: id, n
    dimension makeBase(n)

    makeBase%id = id
end function

type (child) function makeChildData (id, name, n)
use m, only:child
    integer*4, intent(in) :: id, n
    character(*), intent(in) :: name
    dimension makeChildData (n)

    makeChildData%id = id
    makeChildData%name = name
end function
