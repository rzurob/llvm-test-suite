!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/23/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Test the PASS attribute; involve length
!                               type parameters; function result is
!                               parameterized type that has specification
!                               expression for the length type parameter.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamProcComp004
use m
    type(base(:)), target, allocatable :: b1(:)
    class (base(:)), pointer :: b2

    allocate (base(100) :: b1(10), b2)

    b2%mask = (/(mod(i, 2) == 0, i=1, 100)/)
    b2%inverse => negation

    b1(2) = b2%inverse()

    !! verify
    if (.not. associated (b1(2)%inverse, negation)) error stop 1_4

    if (any (b1(2)%mask .neqv. (/(.true._1, .false._1, i=1,50)/))) &
            error stop 2_4

    deallocate (b1, b2)


    !! reallocation for the 2nd test
    allocate (base(20) :: b1(5))

    b2 => b1(5)

    b2%mask = (/(/(.true._1, i=1,10)/), (/(.false._1, i=1,10)/)/)
    b2%inverse => negation

    b1(1) = b2%inverse()

    !! verify
    if (.not. associated(b1(1)%inverse, b1(5)%inverse)) error stop 3_4

    do i = 1, 20
        if (b1(1)%mask(i) .neqv. (.not. b1(5)%mask(i))) error stop 4_4
    end do
end

function negation (b)
use m, only: base
    class(base(*)), intent(in) :: b
    type (base(b%n)) negation

    negation%mask = .not. b%mask
    negation%inverse => b%inverse
end function
