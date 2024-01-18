!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc015.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (unallocated allocatables used in
!                               intrinsic inquiry functions: kind)
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

program falloc015
    integer, parameter :: kind_1 = 1
    integer, parameter :: kind_2 = 2
    integer, parameter :: kind_4 = 4
    integer, parameter :: kind_8 = 8
    integer, parameter :: kind_16 = 16

    !! declarations of intrinsic types with allocatable attributes
    character(len=100), allocatable :: ch1(:)
    integer(kind = kind_8), allocatable :: i1

    double precision, allocatable :: r1(:,:)

    complex (kind_16), allocatable :: cx1

    real, allocatable :: r2(:,:,:)

    logical (2), allocatable :: l1(:)


    if (kind (ch1) /= kind_1) error stop 1_4

    if (kind (i1) /= kind_8) error stop 2_4

    if (kind (r1) /= kind_8) error stop 3_4

    if (kind (cx1) /= kind_16) error stop 4_4

    if (kind (r2) /= kind_4) error stop 5_4

    if (kind (l1) /= kind_2) error stop 6_4
end
