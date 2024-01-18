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
! %GROUP: falloc019.f
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
!*  DATE                       : 09/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (allocation of a pointer creates an
!                               object that implicitly has the TARGET attribute)
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
    contains

    class (*) function transferTarget (x)
        pointer transferTarget
        class (*), target, intent(in) :: x

        transferTarget => x
    end function

    !! note after transfer target to the function result, the lbound is set to 1
    class (*) function transferTargetArray (x)
        pointer transferTargetArray(:)
        class(*), target, intent(in) :: x(:)

        transferTargetArray => x
    end function
end module

program falloc019
use m
use iso_c_binding
    type seq1
        sequence
        integer(2) :: i1, i2 = 1
        integer(8) :: i3
    end type

    type, bind(c) :: bType
        integer(c_short) :: i1
        integer(c_int) :: i2
    end type

    type (seq1), pointer :: s1, s2(:)
    type (bType), pointer :: b1, b2(:)


    !! test the sequence type
    allocate (s1, source=seq1(10, 1, -10))

    s1 => transferTarget (s1)

    if ((s1%i1 /= 10) .or. (s1%i2 /= 1) .or. (s1%i3 /= -10)) error stop 1_4

    !deallocate (s1)

    allocate (s2(-1:0), source=(/seq1(1,i3=1), seq1(10,10,10)/))

    if ((lbound(s2,1)/= -1) .or. (ubound(s2, 1) /= 0)) error stop 2_4

    s2 => transferTargetArray (s2)

    if ((lbound(s2,1)/= 1) .or. (ubound(s2, 1) /= 2)) error stop 3_4

    if ((s2(1)%i1 /= 1) .or. (s2(1)%i2 /= 1) .or. (s2(1)%i3 /= 1)) error stop 4_4
    if ((s2(2)%i1 /= 10) .or. (s2(2)%i2 /= 10) .or. (s2(2)%i3 /= 10)) &
                        error stop 4_4

    !deallocate (s2)


    !! test the bind(C) type
    allocate (b1, source=bTYpe (1, 10))

    b1 => transferTarget (b1)

    if ((b1%i1 /= 1) .or. (b1%i2 /= 10)) error stop 10_4

    !deallocate (b1)


    allocate (b2(0:0), source=(/bType(-1, -10)/))

    b2 => transferTargetArray (b2)

    if ((size (b2) /= 1) .or. (lbound(b2, 1) /= 1)) error stop 11_4

    if ((b2(1)%i1 /= -1) .or. (b2(1)%i2 /= -10)) error stop 12_4

    !deallocate (B2)
end
