! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn028.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited poly pointer
!*                               assigned to sequence target; also sequence
!*                               pointer assigned to unlimited poly-target; also
!*                               involves the common block data)
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

program fpAssgn028
    type seq1(n1,k1,k2)    ! (20,4,8)
        integer, kind :: k1,k2
        integer, len  :: n1
        sequence
        integer(k1)   :: i1
        integer(k2)   :: i2
    end type

    interface
        logical function verifySeq (x1, i1, i2)
            class (*), target, intent(in) :: x1
            integer*4, intent(in) :: i1
            integer*8, intent(in) :: i2
        end function
    end interface

    type (seq1(20,4,8)), target :: s1, s2

    common /test1/ s1, s2

    class (*), pointer ::  x
    type(seq1(:,4,8)), pointer :: s_ptr

    call setUpS1S2

    x => s1

    s_ptr => x

    if ((s_ptr%i1 /= 1) .or. (s_ptr%i2 /= 10)) error stop 1_4

    if (.not. verifySeq (x, 1, 10_8)) error stop 2_4

    x => s2

    s_ptr => x

    if ((s_ptr%i1 /= 20) .or. (s_ptr%i2 /= 200)) error stop 3_4

    if (.not. verifySeq (x, 20, 200_8)) error stop 4_4
end

subroutine setUpS1S2
    implicit type (seq1(20,4,8)) (s)

    type seq1(n2,k3,k4)    ! (20,4,8)
        integer, kind :: k3,k4
        integer, len  :: n2
        sequence
        integer(k3)   :: i1
        integer(k4)   :: i2
    end type

    common /test1/ s1, s2

    s1 = seq1(20,4,8) (1, 10)
    s2 = seq1(20,4,8) (20, 200)
end subroutine

logical function verifySeq (x1, i1, i2)
    class (*), target, intent(in) :: x1
    integer*4, intent(in) :: i1
    integer*8, intent(in) :: i2

    type seq1(n3,k5,k6)    ! (20,4,8)
        integer, kind :: k5,k6
        integer, len  :: n3
        sequence
        integer(k5)      i1
        integer(k6)      i2
    end type

    type (seq1(:,4,8)), pointer :: s

    s => x1

    verifySeq = ((s%i1 == i1) .and. (s%i2 == i2))
end function
