!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005a6_0.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly-pointer
!                               dummy-arg to be only associated with unlimited
!                               poly-pointer actual-arg)
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
    contains

    subroutine createObj (x, x1)
        class (*), pointer, intent(out) :: x
        class (*), intent(in) :: x1

        allocate (x, source=x1)
    end subroutine
end module

program fArg005a6_0
use m
    class (*), pointer :: x1, x2, x3, x4

    nullify (x1, x2, x3, x4)

    !! NOTE the following test can NOT be verified as SELECT TYPE not available
    !yet.

    !! test integer types
    call createObj (x1, 10_1)
    call createObj (x2, 10_2)
    call createObj (x3, 10_8)

    if ((.not. associated (x1)) .or. (.not. associated (x2)) .or. &
        (.not. associated (x3))) error stop 1_4

    deallocate (x1, x2, x3)

    !! test the real types
    call createObj (x1, 1.0_4)
    call createObj (x2, 1.0_8)
    call createObj (x3, 1.0_16)

    if ((.not. associated (x1)) .or. (.not. associated (x2)) .or. &
        (.not. associated (x3))) error stop 2_4

    deallocate (x1, x2, x3)

    !! test the character type
    call createObj (x1, 'xlftest')

    if (.not. associated (x1)) error stop 3_4

    deallocate (x1)

    !! test the logical types
    call createObj (x1, .true._1)
    call createObj (x2, .true._2)
    call createObj (x3, .true._4)
    call createObj (x4, .true._8)

    if ((.not. associated (x1)) .or. (.not. associated (x2)) .or. &
        (.not. associated (x3)) .or. (.not. associated (x4))) error stop 4_4

    deallocate (x1, x2, x3, x4)

    !! test the complex types
    call createObj (x1, (1.0, 2.0))
    call createObj (x2, (1.0d0, 2.0d0))
    call createObj (x3, (1.0q0, 2.0q0))

    if ((.not. associated (x1)) .or. (.not. associated (x2)) .or. &
        (.not. associated (x3))) error stop 5_4

    deallocate (x1, x2, x3)
end
