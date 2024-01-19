! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (use of selected_int_kind and
!                               selected_real_kind inquiry functions in the
!                               type-spec)
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

program falloc001a4
    class (*), pointer :: x, x1, x2(:), x3(:,:), x4, x5(:)

    integer(2), allocatable :: i1(:)

    integer(8), pointer :: i2

    double precision, allocatable :: d1, d2, d3(:)
    real (4), pointer :: r1(:)
    real (16), allocatable :: q1(:,:)


    logical precision_r4, precision_r8, precision_r6

    allocate (integer(selected_int_kind(3)) :: x, i1(2:4))

    allocate (integer(selected_int_kind(11)) :: i2, x1)

    i1 = (/-100_2, 200_2, 900_2/)

    i2 = 98765432108_8

    allocate (real (selected_real_kind (10)) :: d1, x2(10))
    allocate (real(selected_real_kind (6, 70)) :: x3(2,2), d2)
    allocate (real (selected_real_kind (r=100)) :: d3(3), x4)

    allocate (real (selected_real_kind (p=3, r=20)) :: r1(2:3))

    allocate (real (selected_real_kind(p=25)) :: x5(10), q1(2,0:1))

    d1 = .3245678792d3

    d2 = -.523741d68

    d3 = .2462433d96

    r1 = (/.324e19, .524e16/)

    q1 = reshape ((/.3243154652323223253554632q1, .43569824823613549354324q12, &
            .26435092577561532244312q2, .351387664423121324354668793q-10/), &
            (/2,2/))

    !! verify the data

    if ((i1(2) /= -100_2) .or. (i1(3) /= 200_2) .or. (i1(4) /= 900_2)) &
            error stop 1_4

    if (i2 /= 98765432108_8) error stop 2_4

    if (.not. precision_r8 (d1, .3245678792d3)) error stop 3_4
    if (.not. precision_r8 (d2, -.523741d68)) error stop 4_4
    if (.not. precision_r8 (d3(1), .2462433d96)) error stop 5_4
    if (.not. precision_r8 (d3(2), .2462433d96)) error stop 6_4
    if (.not. precision_r8 (d3(3), .2462433d96)) error stop 7_4

    if ((.not. precision_r4 (r1(2), .324e19)) .or. &
        (.not. precision_r4 (r1(3), .524e16))) error stop 8_4


    if (.not. precision_r6 (q1(2, 1), .351387664423121324354668793q-10)) &
            error stop 9_4

    if (.not. precision_r6 (q1(1, 1), .26435092577561532244312q2)) &
            error stop 10_4
end
