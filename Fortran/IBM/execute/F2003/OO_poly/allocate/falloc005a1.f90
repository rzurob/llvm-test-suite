!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (use of defined unary operation as the
!                               source-expr)
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

program falloc005a1
    interface operator (.inverse.)
        real(4) function inverse1 (i)
            integer(4), intent(in) :: i
        end function

        real(8) function inverse2 (i)
            integer(8), intent(in) :: i
        end function
    end interface

    logical(4) precision_r4, precision_r8

    real (8), pointer :: r1
    real (4), allocatable :: r2 (:)

    allocate (r1, source = .inverse. (3_8 + 4_8))
    allocate (r2(2), source = .inverse. (1_4-3_4))

    if (.not. precision_r8 (r1, 1.d0/49.d0)) error stop 1_4

    if (.not. precision_r4 (r2(1), .25_4)) error stop 2_4

    if (.not. precision_r4 (r2(2), .25_4)) error stop 3_4

    deallocate (r1, r2)
end


real(4) function inverse1 (i)
    integer(4), intent(in) :: i

    inverse1 = 1.0

    if (i /= 0)  inverse1 = 1/real(i,4)/real(i,4)
end function


real(8) function inverse2 (i)
    integer(8), intent(in) :: i

    inverse2 = 1.0_8

    if (i /= 0)  inverse2 = 1/real(i,8)/real(i,8)
end function
