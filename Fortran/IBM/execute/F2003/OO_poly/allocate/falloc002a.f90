!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc002a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE ((*) used in type-spec for the
!                               character types)
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

program falloc002a

    character(*), parameter :: c_const = 'xlftest'
    character(5), allocatable :: c1 (:)

    character(len(c_const)), pointer :: c2(:)

    call test1 (c1)

    call test2 (c2, c_const, 5)

    if ((c1(2) /= 'xlfte') .or. (c1(3) /= 'xlfrt')) error stop 1_4

    if (size(c2) /= 5) error stop 2_4
    if (any(c2 /= c_const)) error stop 3_4

    deallocate (c1, c2)

    contains

    subroutine test1 (c)
        character(*), allocatable, intent(inout) :: c (:)

        allocate (character(*) :: c (2:3))

        c(2) = 'xlftest'
        c(3) = 'xlfrte'
    end subroutine

    subroutine test2 (c1, c2, size)
        character(*), pointer, intent(out) :: c1(:)
        character(*), intent(in) :: c2
        integer(4), intent(in) :: size

        allocate (c1(size), source=c2)
    end subroutine
end
