! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv /tstdev/OO_poly/point_assgn/fpAssgn009a3.f
! opt variations: -ql -qdefaultpv

! SCCS ID Information
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
!*
!*  DATE                       : 07/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type seq1(k1,k2)    ! (8,4)
        integer, kind :: k1,k2
        sequence
        integer(k1)      i1
        integer(k2)      i2
    end type

    type seq2(k3,k4)    ! (8,4)
        integer, kind     :: k3,k4
        sequence
        type(seq1(k3,k4))    s1
    end type
end module

module m1
    contains

    subroutine printSeq (x)
        class (*), target, intent(in) :: x(:)

        type seq1(k5,k6)    ! (8,4)
            integer, kind :: k5,k6
            sequence

            integer(k5)      i1
            integer(k6)      i2
        end type

        type (seq1(8,4)), pointer :: s1(:)

        s1 => x

        do i = 1, size (s1)
            print *, s1(i)
        end do
    end subroutine
end module

program fpAssgn009a3
use m
use m1
    type (seq2(8,4)), target :: s2 (10)
    class (*), allocatable, target :: x (:)

    s2 = (/(seq2(8,4)(s1=seq1(8,4)(i1=i,i2 = i*10)), i= 1, 10)/)

    call printSeq (s2(2:5:2))

    call printSeq (s2(::4)%s1)

    allocate (x(2:11), source=s2)

    call printSeq (x)
end
