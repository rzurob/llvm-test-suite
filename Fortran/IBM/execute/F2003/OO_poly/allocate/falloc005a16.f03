! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE(matrix product intrinsics used as the
!                               source-expr in ALLOCATE; use matmul and
!                               dot_product)
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

program falloc005a16
    logical precision_r8

    real(8), allocatable :: r1(:), r2(:,:)
    real(8), pointer :: r3, r4(:)

    real(8) :: rc1(4), rc2(4,2), rc3 (2,4)

    rc1 = (/1.0d0, 2.0d0, 2.0d0, 1.0d0/)

    rc2 = reshape ((/1.d0, 0.d0, 1.d0, 1.d0, 0.d0, 0.d0, 1.d0, 1.d0/), (/4,2/))

    rc3 = reshape ((/2.d0, 1.d0, 1.d0, 2.d0, 1.d0, 2.d0, 2.d0, 1.d0/), (/2,4/))

    !! test the matmul as the source-expr
    allocate (r1(2), source=matmul(rc1, rc2))

    allocate (r2(2,2), source=matmul(rc3, rc2))


    !! test the dot_product as the source-expr
    allocate (r3, source=dot_product(rc1, rc2(:,1)))

    allocate (r4(3:4), source=dot_product(rc3(1,:), rc2(:,2)))


    !! verify the allocation results
    if (.not. precision_r8(r1(1), 4.0d0)) error stop 1_4
    if (.not. precision_r8(r1(2), 3.0d0)) error stop 2_4

    if (.not. precision_r8(r2(1,1), 5.d0)) error stop 3_4
    if (.not. precision_r8(r2(2,1), 4.d0)) error stop 4_4
    if (.not. precision_r8(r2(1,2), 3.d0)) error stop 5_4
    if (.not. precision_r8(r2(2,2), 3.d0)) error stop 6_4

    if (.not. precision_r8(r3, 4.d0)) error stop 7_4
    if (.not. precision_r8(r4(3), 3.d0)) error stop 8_4
    if (.not. precision_r8(r4(4), 3.d0)) error stop 9_4

    deallocate (r1,r2,r3,r4)
end
