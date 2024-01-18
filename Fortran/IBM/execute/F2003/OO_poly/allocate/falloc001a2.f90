!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc001a2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (type-spec used in ALLOCATE statement;
!                               tests double precision and character types)
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

program falloc001a2
    real(kind = 8), pointer :: r1
    class (*), pointer :: x1(:)

    character, pointer :: c1(:)
    character(10), allocatable :: c2(:), c3

    logical precision_r8

    allocate (double precision :: r1, x1(2))

    r1 = 1.0

    deallocate (x1)

    allocate (character (10) :: x1(3), c2(2:3), c3)

    c2 = (/'c2_array_2', 'c2_array_3'/)

    c3 = 'c3_scalar'

    deallocate (x1)
    allocate (character :: c1(-1:0), x1(3))

    c1 = (/'c', '1'/)

    if (.not. precision_r8 (r1, 1.0_8)) error stop 1_4

    if ((lbound(c1,1) /= -1) .or. (ubound(c1,1) /= 0)) error stop 2_4

    if ((lbound(c2,1) /= 2) .or. (ubound(c2,1) /= 3)) error stop 3_4

    if (c3 /= 'c3_scalar') error stop 4_4

    if ((c1(-1) /= 'c') .or. (c1(0) /= '1')) error stop 5_4

    if ((c2(2) /= 'c2_array_2') .or. (c2(3) /= 'c2_array_3')) error stop 6_4
end
