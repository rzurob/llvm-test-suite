!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/11/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (A functional variant based
!                               on dtpOverride003d.)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        contains

        procedure :: p => procP
        procedure, nopass :: q => procQ
    end type

    contains

    function procP (b1, b2)
        class(base(*)), intent(in) :: b1, b2

        procP = -1.0
    end function

    subroutine procQ (b)
        class(base(10)) b

        print *, 'procQ'
    end subroutine
end module

module m1
use m
    type, extends(base) :: child1
        character (n) :: name

        contains

        procedure :: p => procChildP
    end type

    type, extends(base) :: child2
        integer id(n)

        contains

        procedure, nopass :: q => procChildQ
    end type

    contains

    function procChildP (b1, b2)
        class(child1(*)), intent(in) :: b1
        class(base(*)), intent(in) :: b2

        procChildP = sum (iachar([(b1%name(i:i), i=1,b1%n)]))

        select type (b2)
            class is (child1(*))
                procChildP = procChildP + &
                    sum (iachar([(b2%name(i:i), i=1,b2%n)]))
        end select
    end function

    subroutine procChildQ (b)
        class(base(10)) b

        select type (b)
            class is (base(*))
                call procQ (b)

            class is (child2(*))
                print *, b%id
        end select
    end subroutine
end module


program dtpOverride003
use m1
    implicit none

    type(base(10)) b1
    type(base(:)), allocatable :: b2

    type(child1(10)) c1
    type(child1(:)), pointer :: c2

    type(child2(10)), allocatable :: c3
    type(child2(:)), pointer :: c4

    class(base(10)), pointer :: b3
    class(base(:)), allocatable :: b4, b5

    real :: r1, r2
    logical(4), external :: precision_r4
    integer i

    !! test base type's bindings
    allocate(base(100) :: b2)

    r1 = b1%p(b2)

    call b2%q(b1)       !<-- call procQ

    if (.not. precision_r4 (r1, -1.0_4)) error stop 1_4

    !! test child1's bindings
    allocate (child1(5) :: c2)

    allocate (c3)

    c3%id = [(i, i = 1, 10)]

    c2%name = 'xlf'
    c1%name = 'F2003 test'

    r1 = c1%p(c2)
    r2 = c2%p(b2)

    if (.not. precision_r4(r1, 1141.0_4)) error stop 2_4

    if (.not. precision_r4(r2, 394.0_4)) error stop 3_4

    if (.not. precision_r4(c2%p(c3), 394.0_4)) error stop 4_4

    call c1%q(c1)       !<-- indrited binding: procQ

    !! test child2's bindings
    allocate (child2(15) :: c4)

    call c4%q(c3)       !<-- call procChildQ with child2 data-arg
    call c3%q(c1)       !<-- call procChildQ --> procQ

    if (.not. precision_r4 (-1.0_4, c3%p(c4))) error stop 5_4


    !! test polymorphic entities
    allocate (b3, source=child1(10)('xlftest'))
    allocate (b4, source=child1(15)(''))
    allocate (child2(5) :: b5)

    if (.not. precision_r4(b3%p(b4), 1354.0_4)) error stop 6_4

    if (.not. precision_r4(480.0_4, b4%p(b5))) error stop 7_4

    call b5%q(c1)       !<-- call procChildQ --> procQ

    call b5%q(c3)       !<-- call procChildQ with child2 data-arg
end
