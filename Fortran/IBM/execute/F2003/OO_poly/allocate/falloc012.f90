!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc012.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (if an error condition occurs during
!                               the execution of the allocate stmt, the stat var
!                               becomes defined with a processor-dependent
!                               integer value and each allocate-object will
!                               have a processor-dependent status)
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

program falloc012
    class (*), allocatable :: x0, x1(:), x2(:,:)

    class (*), pointer :: x3
    integer error

    allocate (x0, source=(1.0, 0.5))
    nullify (x3)

    !! reallocate x0 will cause error condition; but the allocation for x1 and
    !x3 will succeed
    allocate (integer(kind=8) :: x3, x0, x1(2:4), stat=error)

    if (error /= 2) error stop 1_4

    if (.not. associated(x3)) error stop 1_4

    if (.not. allocated (x1)) error stop 2_4

    if ((lbound(x1, 1) /= 2) .or. (ubound(x1, 1) /= 4)) error stop 3_4


    deallocate (x3)

    !! again, this allocate statement will fail and only x3 and x2 get allocated

    error = -100
    allocate (complex(8) :: x1(2), x2(2, 0:1), x3, stat=error)

    if (error /= 2) error stop 4_4

    if (.not. associated(x3)) error stop 5_4

    if (.not. allocated(x2)) error stop 6_4

    if (any(lbound(x2) /= (/1, 0/)) .or. any (ubound(x2) /= (/2,1/))) error stop 7_4

    deallocate (x1, x2, x3, x0)
end
