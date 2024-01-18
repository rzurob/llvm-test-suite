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
! %GROUP: fArg005a3.f
! %VERIFY: fArg005a3.out:fArg005a3.vf
! %STDIN:
! %STDOUT: fArg005a3.out
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
!*  DATE                       : 05/05/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (poly-pointer dummy-arg's
!*                               association; scalars and arrays)
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

    interface
        subroutine createBase (b, id, name)
        import base
            class (base), pointer, intent(out) :: b
            integer*4, intent(in) :: id
            character(*), intent(in), optional :: name
        end subroutine

        subroutine createBaseArray (b, id, name, arraySize)
        import base
            class (base), pointer, intent(out) :: b(:)
            integer*4, intent(in) :: id, arraySize
            character(*), intent(in), optional :: name
        end subroutine
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

program fArg005a3
use m
    class (base), pointer :: b1, b2(:)

    type (child), target :: c1, c2(2:4)

    b1 => c1
    b2 => c2

    call createBase (b1, 10, name='b1_pointer')

    call b1%print

    deallocate (b1)

    call createBase (b1, 15)

    call b1%print

    call createBaseArray (b2, 20, arraySize=2)

    if (size(b2) /= 2) error stop 1_4

    call b2(1)%print
    call b2(2)%print

    deallocate (b2)

    call createBaseArray (b2, 25, 'b2_array_of_3', 3)

    if (size(b2) /= 3) error stop 2_4

    call b2(1)%print
    call b2(2)%print
    call b2(3)%print

    deallocate (b1, b2)
end


subroutine createBase (b, id, name)
use m, only : base, child
    class (base), pointer, intent(out) :: b
    integer*4, intent(in) :: id
    character(*), intent(in), optional :: name

    if (present (name)) then
        allocate (b, source=child(id, name))
    else
        allocate (b)
        b%id = id
    end if
end subroutine


subroutine createBaseArray (b, id, name, arraySize)
use m, only : base, child
    class (base), pointer, intent(out) :: b(:)
    integer*4, intent(in) :: id, arraySize
    character(*), intent(in), optional :: name

    if (present (name)) then
        allocate (b(arraySize), source=child(id, name))
    else
        allocate (b(arraySize))
        b%id = id
    end if
end subroutine
