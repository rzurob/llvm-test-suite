!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc026a2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (deallocate a disassociated pointer
!                               will result in an error condition)
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
    type base
        integer id

        contains

        final :: finalizeBase
    end type

    type base1
        contains

        final finalizeBase1
    end type

    type (base), pointer :: b1_m => null()
    class (base1), pointer :: b2_m(:) => null()

    type (base1), pointer :: b3_m => null()
    class (base), pointer :: b4_m => null()

    contains

    subroutine finalizeBase1 (b)
        type (base1), intent(in) :: b
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(in) :: b
    end subroutine
end module

program falloc026a1
use m
    integer error

    !! test the pointers that are initialized to be disassociated
    deallocate (b1_m, stat=error)

    if (error == 0) error stop 1_4

    deallocate (b3_m, stat=error)

    if (error == 0) error stop 2_4

    deallocate (b2_m, stat = error)

    if (error == 0) error stop 3_4

    deallocate (b4_m, stat=error)

    if (error == 0) error stop 4_4

    !! test disassociated pointer via deallocate
    allocate (b2_m(10))

    deallocate (b2_m, stat=error)

    if (error /= 0) error stop 5_4

    deallocate (b2_m, stat=error)

    if (error == 0) error stop 6_4

    ! test the disassociated pointer via nullify
    allocate (b4_m)

    nullify (b4_m)

    deallocate (b4_m, stat= error)

    if (error == 0) error stop 7_4

    !! try null() function
    allocate (b4_m)

    b4_m => null()

    deallocate (b4_m, stat= error)

    if (error == 0) error stop 8_4
end
