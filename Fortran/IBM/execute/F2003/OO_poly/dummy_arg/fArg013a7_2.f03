! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/20/2005
!*
!*  DESCRIPTION                : argument association (TARGET attribute and
!                               assumed-shape array)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fArg013a7_2
    class (*), pointer :: x1(:,:)

    integer(8), target :: i1(100,100)

    call associateX (i1(::2,2::2))

    if (.not. associated (x1, i1(3::4,6::6))) error stop 1_4

    contains

    subroutine associateX (x)
        class (*), target :: x(0:,2:)

        x1 => x(1::2,4::3)
    end subroutine
end
