!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn024a2.f
! %VERIFY: fpAssgn024a2.out:fpAssgn024a2.vf
! %STDIN:
! %STDOUT: fpAssgn024a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer as
!*                               function return)
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
        character*15 :: name = ''

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        integer*4 :: id = 0

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase(b)
        class (base), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%name, b%id
    end subroutine
end module

program fpAssgn024a2
use m
    interface makeData
        function produceBasePtr (c)
        use m
            class (base), pointer :: produceBasePtr
            character(*), intent(in) :: c
        end function

        function produceChildPtr (c, i)
        use m
            class (base), pointer :: produceChildPtr
            character(*), intent(in) :: c
            integer*4, intent(in) :: i
        end function
    end interface

    type (base), pointer :: b1
    class (base), pointer :: b2

    b1 => makeData ('b1 test')

    b2 =>  makeData ('b1 test')

    call b1%print
    call b2%print

    deallocate (b1, b2)

    b1 => makeData ('test2', 10)

    b2 => makeData ('test2', 10)

    call b1%print
    call b2%print

    deallocate (b2)
end

function produceBasePtr (c)
use m
    class (base), pointer :: produceBasePtr
    character(*), intent(in) :: c

    allocate (produceBasePtr)

    produceBasePtr%name = c
end function

function produceChildPtr (c, i)
use m
    class (base), pointer :: produceChildPtr
    character(*), intent(in) :: c
    integer*4, intent(in) :: i

    type (child), pointer :: tmp
    allocate (tmp)

    tmp%name = c
    tmp%id = i

    produceChildPtr => tmp
end function
