!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005.f
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
!*  DESCRIPTION                : ALLOCATE (constants used in source-expr;
!                               non-poly allocate-object)
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

program falloc005
    character(20), pointer :: c1, c2
    integer(8), allocatable :: i1
    complex(8), allocatable :: cx1(:)

    logical(4) precision_x6

    allocate (c1, source='test1')
    allocate (i1, source=100_8)
    allocate (cx1(2), source= (1.0d0, 2.0d0))

    allocate (c2, source='xlftest 101'(4:7))

    if (c1 /= 'test1') error stop 1_4

    if (i1 /= 100_8) error stop 2_4

    if (.not. precision_x6 (cx1(1), (1.0d0, 2.0d0))) error stop 3_4
    if (.not. precision_x6 (cx1(2), (1.0d0, 2.0d0))) error stop 4_4

    if (c2 /= 'test') error stop 5_4

    deallocate (c1, i1, cx1, c2)
end
