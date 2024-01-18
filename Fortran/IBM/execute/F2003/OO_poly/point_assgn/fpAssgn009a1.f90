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
! %GROUP: fpAssgn009a1.f
! %VERIFY: fpAssgn009a1.out:fpAssgn009a1.vf
! %STDIN:
! %STDOUT: fpAssgn009a1.out
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
!*  DATE                       : 04/01/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (sequenec type on LHS
!*                               of the assignment while class(*) type as RHS)
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
        integer*8 :: i2
        integer*2, pointer :: i3 => null()
    end type

    contains

    subroutine printSeq (x)
        class (*), intent(in), target :: x

        type (seq1), pointer :: s

        s => x

        if (associated(s%i3)) then
            print *, s%i1, s%i2, s%i3
        else
            print *, s%i1, s%i2, 'null'
        end if
    end subroutine
end module

program fpAssgn009a1
use m, only : printSeq

    interface
        subroutine assgnI3 (x, i)
            class (*), intent(inout), target :: x
            integer*2, target, intent(in) :: i
        end subroutine
    end interface

    type seq1
        sequence
        integer*4 :: i1
        integer*8 :: i2
        integer*2, pointer :: i3 => null()
    end type

    type (seq1) :: s1
    integer*2, target :: i1

    i1 = 100

    s1 = seq1 (1, 10)

    call printSeq (s1)

    call assgnI3 (s1, i1)

    call printSeq (s1)
end

subroutine assgnI3 (x, i)
    class (*), intent(inout), target :: x
    integer*2, target, intent(in) :: i

    type seq1
        sequence
        integer*4 :: i1
        integer*8 :: i2
        integer*2, pointer :: i3 => null()
    end type

    type (seq1), pointer :: s1

    s1 => x

    s1%i3 => i
end subroutine
