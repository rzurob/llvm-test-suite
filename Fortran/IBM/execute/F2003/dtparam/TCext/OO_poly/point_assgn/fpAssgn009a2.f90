! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn009a2.f
! opt variations: -qnol -qdeferredlp -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (sequence type pointer
!                               assigned to unlimited poly-target; arrays)
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
    type seq1(n1,k1,k2,k3)    ! (20,4,4,2)
        integer, kind        :: k1,k2,k3
        integer, len         :: n1
        sequence
        integer(k1)          :: i1
        integer(k2)          :: i2
        integer(k3), pointer :: i3 => null()
    end type

    contains

    subroutine printSeq (x)
        class (*), intent(in), target :: x(:)

        type (seq1(20,4,4,2)), pointer :: s(:)

        s => x

        print *, (s(i)%i1, s(i)%i2, s(i)%i3, i=1,size(s))
    end subroutine
end module

program fpAssgn009a2
use m, only : printSeq
    class (*), pointer :: x1(:)

    type seq1(n2,k4,k5,k6)    ! (20,4,4,2)
        integer, kind        :: k4,k5,k6
        integer, len         :: n2
        sequence
        integer(k4)          :: i1
        integer(k5)          :: i2
        integer(k6), pointer :: i3 => null()
    end type

    type (seq1(20,4,4,2)), target :: s1(10)
    integer*2, target :: i1(10)

    i1 = (/(i*100, i=1,10)/)

    s1 = (/(seq1(20,4,4,2) (i1=i, i2=i*10, i3 = i1(i)), i=1,10)/)

    call printSeq (s1)

    x1 => s1

    call printSeq (x1)

end
