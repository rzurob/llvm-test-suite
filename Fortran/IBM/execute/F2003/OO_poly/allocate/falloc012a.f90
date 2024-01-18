!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc012a.f
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

program falloc012a
    integer(8), allocatable :: i0, i1(:)

    character(15), allocatable :: c2(:,:)

    integer(4) error

    allocate (i0, i1(10), c2(2, 3))

    !! allocate allocated allocatable causes failure
    allocate (i0, stat=error, source=-100_8)

    if (error /= 2) error stop 1_4

    allocate (i1(2), source=-1_8, stat=error)

    if (error /= 2) error stop 2_4

    allocate (c2(10,100), source='xlftest', stat=error)

    if (error /= 2) error stop 3_4

    if (size(i1) /= 10) error stop 4_4

    if (any (shape (c2) /= (/2,3/))) error stop 5_4
end
