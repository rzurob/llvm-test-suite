! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass020.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/03/2005
!*
!*  DESCRIPTION                : CLASS keyword (entry statement in the defined
!                               assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
    type base(k1)    ! (8)
        integer, kind            :: k1
        real(k1), pointer        :: d1
        integer(k1), allocatable :: i1
    end type

    interface assignment(=)
        subroutine assgnVal (b1, b2)
        import base
            class (base(8)), intent(out) :: b1
            class (base(8)), intent(in) :: b2
        end subroutine

        subroutine assgnReal (b1, d1)
        import base
            real(8), intent(in) :: d1
            class(base(8)), intent(out) :: b1
        end subroutine

        subroutine assgnInt (b1, i1)
        import base
            integer(8), intent(in) :: i1
            class(base(8)), intent(out) :: b1
        end subroutine
    end interface
end module

subroutine assgnVal (b1, b2)
use m, only: base
    class (base(8)), intent(out) :: b1
    class (base(8)), intent(in) :: b2

    real(8), intent(in) :: d1
    integer(8), intent(in) :: i1

    if (associated (b2%d1))  allocate (b1%d1, source=b2%d1)
    if (allocated (b2%i1)) allocate (b1%i1, source=b2%i1)

    return

    entry assgnReal (b1, d1)
        allocate (b1%d1, source=d1)
        return

    entry assgnInt (b1, i1)
        nullify (b1%d1)
        allocate (b1%i1, source=i1)
end subroutine

program fclass020
use m
    class (base(8)), allocatable :: b1
    real(8), target :: dd1
    logical(4) precision_r8

    allocate (b1, source=base(8)(dd1, 1000))

    !! test assgnReal
    b1 = 1.0d0

    if (.not. precision_r8(b1%d1, 1.0d0)) error stop 1_4
    if (allocated (b1%i1)) error stop 2_4

    !! test assgnInt
    b1 = -10_8

    if (b1%i1 /= -10) error stop 3_4

    if (associated (b1%d1)) error stop 4_4

    !! test assgnVal
    dd1 = 2.5d0

    b1 = base(8) (dd1, i1 = -100)

    if ((.not. associated (b1%d1)) .or. (.not. allocated (b1%i1))) error stop 5_4

    if (.not. precision_r8 (b1%d1, 2.5d0)) error stop 6_4
    if (b1%i1 /= -100) error stop 7_4
end
