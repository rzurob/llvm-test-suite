! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn009a1.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type seq1(k1,k2,k3)    ! (4,8,2)
        integer, kind        :: k1,k2,k3
        sequence
        integer(k1)          :: i1
        integer(k2)          :: i2
        integer(k3), pointer :: i3 => null()
    end type

    contains

    subroutine printSeq (x)
        class (*), intent(in), target :: x

        type (seq1(4,8,2)), pointer :: s

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

    type seq1(k4,k5,k6)    ! (4,8,2)
        integer, kind        :: k4,k5,k6
        sequence
        integer(k4)          :: i1
        integer(k5)          :: i2
        integer(k6), pointer :: i3 => null()
    end type

    type (seq1(4,8,2)) :: s1
    integer*2, target :: i1

    i1 = 100

    s1 = seq1(4,8,2) (1, 10)

    call printSeq (s1)

    call assgnI3 (s1, i1)

    call printSeq (s1)
end

subroutine assgnI3 (x, i)
    class (*), intent(inout), target :: x
    integer*2, target, intent(in) :: i

    type seq1(k7,k8,k9)    ! (4,8,2)
        integer, kind        :: k7,k8,k9
        sequence
        integer(k7)          :: i1
        integer(k8)          :: i2
        integer(k9), pointer :: i3 => null()
    end type

    type (seq1(4,8,2)), pointer :: s1

    s1 => x

    s1%i3 => i
end subroutine
