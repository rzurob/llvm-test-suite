! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/31/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (unallocated allocatables used in the
!                               intrinsic inquiry function digits())
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

program falloc015a
    integer(8), allocatable :: i1(:,:)
    double precision, allocatable :: d1(:)
    integer, allocatable :: i2
    real, allocatable :: r1(:,:,:,:,:,:,:)

    if (digits(i1) /= 63) error stop 1_4
    if (digits(d1) /= 53) error stop 2_4
    if (digits(x = i2) /= 31) error stop 3_4
    if (digits(x = r1) /= 24) error stop 4_4
end
