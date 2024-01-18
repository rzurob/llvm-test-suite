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
! %GROUP: falloc018a.f
! %VERIFY: falloc018a.out:falloc018a.vf
! %STDIN:
! %STDOUT: falloc018a.out
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
!*  DATE                       : 09/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (when the allocation status of an
!                               allocatable variable changes, the allocation
!                               status of any associated allocatable variable
!                               changes accordingly. Section 6.3.1.1, page 113,
!                               line 13)
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
        integer(4) :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (base), allocatable :: b1_m(:)

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

subroutine test1 (b)
    use m, b1 => b1_m
    class (base), allocatable :: b

    if (allocated (b1)) then
        print *, shape (b1)
        deallocate(b1)
    else
        allocate (child:: b1(0:1))
    end if

    if (allocated (b))  then
        call b%print
    else
        allocate (b, source=child(100, 'new'))
    end if
end subroutine


program falloc018a
use m
    interface
        subroutine test1 (b)
        use m
            class (base), allocatable :: b
        end subroutine

        subroutine test2 (b1_m)
        use m, b1=>b1_m
            class (base), allocatable :: b1_m(:)
        end subroutine
    end interface

    class (base), allocatable :: b1

    call test1(b1)

    if ((.not. allocated (b1)) .or. (.not. allocated (b1_m))) error stop 1_4

    call b1%print

    if ((lbound(b1_m,1) /= 0) .or. (ubound(b1_m,1) /= 1)) error stop 2_4

    call b1_m(0)%print
    call b1_m(1)%print

    call test1 (b1)

    if (allocated (b1_m) .or. (.not. allocated (b1))) error stop 3_4

    print *, 'second test'

    call test2(b1_m)

    if (.not. allocated (b1_m)) error stop 4_4

    if ((lbound(b1_m,1) /= 0) .or. (ubound(b1_m,1) /= 1)) error stop 5_4

    call b1_m(0)%print
    call b1_m(1)%print
end


subroutine test2 (b1_m)
use m, b1 => b1_m
    class (base), allocatable :: b1_m(:)

    if (allocated (b1_m) .neqv. allocated (b1)) error stop 10_4

    if (.not. allocated (b1_m)) then
        allocate (b1_m(0:1), source=(/child(1,'new1'), child(2,'new2')/))
    else
        deallocate (b1_m)
    end if
end subroutine
