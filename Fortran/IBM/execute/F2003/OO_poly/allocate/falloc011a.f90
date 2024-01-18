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
! %GROUP: falloc011a.f
! %VERIFY: falloc011a.out:falloc011a.vf
! %STDIN:
! %STDOUT: falloc011a.out
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
!*  DATE                       : 08/16/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (stat= with source=)
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
        character(15) :: name = 'default'

        contains

        procedure, pass(b) :: print => printBase
    end type

    type, extends(base) :: child
        integer(8) :: id = -1_8

        contains

        procedure, pass(b) :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%name, b%id
    end subroutine
end module

program falloc011a
use m
    class (base), allocatable :: b1(:)
    class (*), pointer :: x0, x1, x2(:), x3(:,:)

    complex(8), allocatable, target :: cx(:)

    integer stat
    integer :: error = 100

    logical precision_x6

    allocate (b1 (100:102), stat=stat, source=child('test101', 10))

    if (stat /= 0) error stop 1_4

    call b1(100)%print
    call b1(101)%print
    call b1(102)%print

    allocate (cx(2:3), source=(1.5d0, 0.5d0), stat=stat)

    if (stat /= 0) error stop 2_4

    if (.not. precision_x6 (cx(2), (1.5d0, 0.5d0))) error stop 3_4
    if (.not. precision_x6 (cx(3), (1.5d0, 0.5d0))) error stop 4_4

    allocate (x1, stat=error, source=100_4)

    if (error /= 0) error stop 5_4

    x0 => cx(2)

    allocate(x2 (error:10), source=x0, stat=error)

    if (error /= 0) error stop 6_4

    if (any (shape (x2) /= (/11/))) error stop 7_4

    allocate (x3(2, 3), stat= error, source=b1(101))

    if (error /= 0) error stop 8_4
end
