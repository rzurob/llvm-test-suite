!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc009a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (if lower bound is greater than upper
!                               bound, then the extend in that dimension is
!                               zero)
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

    integer(kind=8) :: s1
    integer(kind = 1) :: s2
    real(8), allocatable :: r1(:), r2(:)
    complex(8), pointer :: cx(:,:)

    s1 = -9_8
    s2 = -10_1

    allocate (r1(s1), source = 1.0d0)
    allocate (real(8) :: r2(s2))

    allocate (cx(s1, s2), source= (1.d0, .5d0))

    if (size(r1) /= 0) error stop 1_4
    if (size(r2) /= 0) error stop 2_4

    if ((.not. allocated(r1)) .or. (.not. allocated(r2))) error stop 3_4

    if (.not. associated (cx)) error stop 4_4

    if (any (shape (cx) /= (/0,0/))) error stop 5_4

    if (size(cx) /= 0) error stop 6_4

    if ((lbound(r1, 1) /= 1) .or. (lbound(r2,1) /= 1)) error stop 7_4

    if (any (lbound(cx) /= 1)) error stop 8_4

    if (any (ubound(cx) /= 0)) error stop 9_4

    if ((ubound(r1,1) /= 0) .or. (ubound(r2,1) /= 0)) error stop 10_4

end
