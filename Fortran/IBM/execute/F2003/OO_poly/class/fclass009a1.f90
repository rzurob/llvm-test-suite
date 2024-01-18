!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass009a1.f
! %VERIFY: fclass009a1.out:fclass009a1.vf
! %STDIN:
! %STDOUT: fclass009a1.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 12/13/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : CLASS keyword (operator(+) defined for derived
!                               type; use poly function return; test scalars)
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
        integer(4), allocatable :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    interface operator(+)
        class (base) function addB1B2 (b1, b2)
        import base
            class (base), intent(in) :: b1, b2
            allocatable :: addB1B2
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (allocated(b%id)) then
            print *, b%id
        else
            print *, 'id not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        if (allocated(b%id)) then
            print *, b%id, b%name
        else
            print *, 'id not allocated; ', b%name
        end if
    end subroutine
end module

program fclass009a1
use m
    class (base), allocatable :: b1

    allocate (b1)

    associate (x => b1 + base (1))
        call x%print
    end associate

    associate (x => child(10, 'xlftest') + child(20, 'team'))
        call x%print
    end associate

    deallocate (b1)
    allocate (b1, source=child(100, 'compiler'))

    associate (x => b1 + child(-10, 'test') + b1)
        call x%print
    end associate
end


class (base) function addB1B2 (b1, b2)
use m, only: base, child
    class (base), intent(in) :: b1, b2
    allocatable :: addB1B2

    integer id1, id2
    character(20) :: localName

    id1 = 0
    id2 = 0

    if (.not. same_type_as (b1, b2)) error stop 1_4

    if (allocated (b1%id)) id1 = b1%id
    if (allocated (b2%id)) id2 = b2%id

    select type (b1)
        type is (base)
            allocate (addB1B2, source=base(id1+id2))
        type is (child)
            select type (b2)
                type is (child)
                    localName = trim(b1%name) // ' ' // trim(b2%name)

                    allocate (addB1B2, source=child(id1+id2, localName))
                class default
                    error stop 8_4
            end select
        class default
            error stop 10_4
    end select
end function
