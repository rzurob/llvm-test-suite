!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc001a6.f
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
!*  DESCRIPTION                : ALLOCATE (kind() inquiry function used in
!                               type-spec for real and complex types in allocate
!                               statement)
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

program falloc001a6
    class (*), pointer :: x1, x2(:)

    real*8, parameter :: pi = 3.14159265358d0

    real*4, pointer :: r1
    real (8), allocatable :: r2(:)

    complex*8, allocatable :: cx1
    complex*16, pointer :: cx2(:)

    logical precision_r4, precision_r8, precision_x8, precision_x6

    allocate (real(kind(pi)) :: x1, r2(2:3))

    allocate (real(kind(1_4)) :: r1)

    allocate (complex(kind(cmplx(1.0e1, .5e1))) :: cx1, x2(2))

    allocate (complex(kind = kind(cmplx(pi, kind=kind(pi)))) :: cx2 (-2:0))

    r1 = 3.14159

    r2 = (/pi, 2.0d0*pi/)

    cx1 = (r1, r1)

    cx2 = cmplx(pi, kind=kind(pi))

    if (.not. precision_r4 (r1, 3.14159)) error stop 1_4

    if (.not. precision_r8 (r2(2), pi)) error stop 2_4

    if (.not. precision_r8 (r2(3), 2.d0*pi)) error stop 3_4

    if (.not. precision_x8 (cx1, cmplx(3.14159, 3.14159, kind=4))) error stop 4_4

    if (.not. precision_x6 (cx2(-1), (pi, 0.d0))) error stop 5_4
    if (.not. precision_x6 (cx2(-1), cx2(0))) error stop 6_4
    if (.not. precision_x6 (cx2(-2), (pi, 0.d0))) error stop 7_4

end
