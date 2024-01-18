! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr044.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (sequence pointer
!*                               component initialization in struct_constr using
!*                               class(*) target)
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
    type seq1(k1,k2)    ! (2,4)
        integer, kind :: k1,k2
        sequence
        integer(k1)   :: i1
        integer(k2)   :: i2
    end type

    type base(k3,k4)    ! (4,2)
        integer, kind              :: k3,k4
        integer(k3)                :: id = 0
        type(seq1(k4,k3)), pointer :: s1 => null()
    end type
end module

program fconstr044
use m
    type (base(4,2)) :: b1

    class (*), pointer :: x

    nullify (x)

    b1 = base(4,2) (id = 10, s1 = x)

    if (associated (b1%s1)) error stop 1_4

    call assgnS1 (b1)

    if (b1%id /= 10) error stop 2_4

    if (.not. associated (b1%s1)) error stop 3_4

    if ((b1%s1%i1 /= 10) .or. (b1%s1%i2 /= 100)) error stop 4_4
end

subroutine assgnS1 (b1)
use m, only: base
    type (base(4,2)), intent(inout) :: b1

    class (*), pointer :: x

    type seq1(k5,k6)    ! (2,4)
        integer, kind :: k5,k6
        sequence
        integer(k5)   :: i1
        integer(k6)   :: i2
    end type

    type (seq1(2,4)), target :: s11 = seq1(2,4) (10, 100)

    x => s11

    b1 = base(4,2) (b1%id, x)
end subroutine
