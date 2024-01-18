!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc003a.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 07/22/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (scalar source-expr used for rank2
!                               arrays; use unlimited poly type and complex
!                               types)
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

program falloc003a
    class (*), allocatable :: x (:,:)

    complex*16, pointer :: cx(:,:)

    real(kind = 8) :: r1(2,2), r2(4)

    allocate (x (2, 2), source=(1.0d0, 1.0d0))

    allocate (cx (2, 2), source=(1.0d0, 1.0d0))

    if (any (shape (x) /= (/2,2/))) error stop 1_4

    r1 = real(cx)
    r2 = reshape (aimag(cx), (/4/))

    if (.not. equal_r8 (r1, 1.d0)) error stop 2_4

    if (.not. equal_r8 (r2, 1.d0)) error stop 3_4

    contains

    !! we only check for the first 4 element
    logical function equal_r8 (r1, r2)
        real(8), intent(in) :: r1(*), r2

        real(8), parameter :: r_const = 5.d-15

        equal_r8 = .true.

        do i = 1, 4
            equal_r8 = equal_r8 .and. (abs (r1(i) - r2) / (r1(i) + r2)) < r_const
        end do
    end function
end
