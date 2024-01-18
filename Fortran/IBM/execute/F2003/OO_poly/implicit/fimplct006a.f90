!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fimplct006a.f
! %VERIFY: fimplct006a.out:fimplct006a.vf
! %STDIN:
! %STDOUT: fimplct006a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT (implied poly-data by external
!*                               function result)
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
        integer*4 :: id = 0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*15 :: name = 'no-name'

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
end module

module m1
use m, only : base, child
    type (child) :: c1_m = child (10, 'c1')
    target c1_m
end module

program fimplct006a
use m1
    implicit class (base) (b)

    interface makeData
        function makeBaseObj (i)
        use m1
            implicit class (base) (m)
            intent(in) :: i
            pointer makeBaseObj
        end function

        function makeChildObj (i, c)
        use m
            implicit class (child) (m)
            intent(in) :: i
            character(*), intent(in) :: c
            pointer makeChildObj
        end function
    end interface

    pointer :: b_ptr

    b_ptr => makeData(10)

    call b_ptr%print

    deallocate (b_ptr)

    b_ptr => makeData(100, 'test1')

    call b_ptr%print

    deallocate (b_ptr)

    b_ptr => c1_m

    call b_ptr%print
end

function makeBaseObj (i)
use m1
    implicit class (base) (m)
    intent(in) :: i
    pointer makeBaseObj

    allocate (makeBaseObj, source=base(i))
end function

function makeChildObj (i, c)
use m
    implicit class (child) (m)
    intent(in) :: i
    character(*), intent(in) :: c
    pointer makeChildObj

    allocate (makeChildObj, source=child(i, c))
end function
