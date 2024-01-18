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
! %GROUP: fArg013a2.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
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
!*  DATE                       : 11/25/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (TARGET attribute for the
!                               dummy arg)
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
    type seq1
        sequence

        integer*4 :: i1
        integer*4 :: i2
    end type

    type, BIND(C) :: bind1
        integer*4 :: i1
    end type

    type (seq1), pointer :: s1_m
    type (bind1), pointer :: bd1_m

    class (*), pointer :: x1

    contains

    subroutine associatePtr1 (x)
        class (*), target, intent(in) :: x

        x1 => x
    end subroutine

    subroutine associateSeqPtr (s, x)
        type seq1
            sequence

            integer*4 :: i1
            integer*4 :: i2
        end type

        class (*), target, intent(out) :: x
        type (seq1), pointer, intent(out) :: s

        !! x has undefined values at this point
        s => x

        s = seq1 (1, 2)
    end subroutine

end module


program fArg013a2
use m
    integer*4, target :: i1

    character*20, target :: ch1

    type (seq1), target :: s1
    type (bind1), target :: bd1

    s1 = seq1 (2, 3)
    bd1 = bind1(100)

    call associatePtr1 (i1)

    if (.not. associated (x1, i1)) error stop 1_4

    call associatePtr1 (ch1)

    if (.not. associated (x1, ch1)) error stop 2_4

    call associatePtr1 (s1)

    if (.not. associated (x1, s1)) error stop 3_4

    !! self-assgn is not a problem
    call associatePtr1 (x1)

    if (.not. associated (x1, s1)) error stop 4_4

    s1_m => x1

    if ((s1_m%i1 /= 2) .or. (s1_m%i2 /= 3)) error stop 5_4

    !! bind(c) type
    call associatePtr1 (bd1)

    if (.not. associated (x1, bd1)) error stop 6_4

    bd1_m => x1

    if (bd1_m%i1 /= 100) error stop 7_4




    !! test associateSeqPtr call
    call associateSeqPtr (s1_m, s1)

    if (.not. associated (s1_m, s1)) error stop 10_4

    if ((s1_m%i1 /= 1) .or. (s1_m%i2 /= 2)) error stop 11_4

    !! s1_m, self-assgn call; because the FE creates temporay descriptors for
    !the dummy-arg, it shall be safe to call this way
    call associateSeqPtr (s1_m, s1_m)

    if ((s1_m%i1 /= 1) .or. (s1_m%i2 /= 2)) error stop 12_4

end
