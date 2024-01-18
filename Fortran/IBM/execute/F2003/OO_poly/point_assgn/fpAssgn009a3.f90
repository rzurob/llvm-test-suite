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
! %GROUP: fpAssgn009a3.f
! %VERIFY: fpAssgn009a3.out:fpAssgn009a3.vf
! %STDIN:
! %STDOUT: fpAssgn009a3.out
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
!*  DATE                       : 07/07/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : data pointer assignment (sequence type pointer
!                               assigned to unlimited poly-target; use arrays
!                               and dummy-args)
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
        integer(8) i1
        integer(4) i2
    end type

    type seq2
        sequence
        type (seq1) s1
    end type
end module

module m1
    contains

    subroutine printSeq (x)
        class (*), target, intent(in) :: x(:)

        type seq1
            sequence

            integer(8) i1
            integer(4) i2
        end type

        type (seq1), pointer :: s1(:)

        s1 => x

        do i = 1, size (s1)
            print *, s1(i)
        end do
    end subroutine
end module

program fpAssgn009a3
use m
use m1
    type (seq2), target :: s2 (10)
    class (*), allocatable, target :: x (:)

    s2 = (/(seq2(s1=seq1(i1=i,i2 = i*10)), i= 1, 10)/)

    call printSeq (s2(2:5:2))

    call printSeq (s2(::4)%s1)

    allocate (x(2:11), source=s2)

    call printSeq (x)
end
