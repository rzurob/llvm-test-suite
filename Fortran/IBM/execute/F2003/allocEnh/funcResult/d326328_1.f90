!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/05/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 326328)
!                               This is the original version of test case.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    real(8), allocatable :: d1(:), d2(:)

    real(8) x(10), y(10, 8)

    logical(4), external :: precision_r8

    x = [(log(i*1.5d0), i=1,10)]

    do i = 1, 10
        y(i,:) = [(log(j*1.0d0), j=i,i+7)]
    end do

    d1 = matmul(x, y)

    d2 = [(sum(x(:)* y(:,i)), i=1,8)]

    if ((.not. allocated(d1)) .or. (size(d1) /= 8)) error stop 1_4

    if ((.not. allocated(d2)) .or. (size(d2) /= 8)) error stop 2_4

    do i = 1, 8
        if (.not. precision_r8(d1(i), d2(i))) error stop 3_4
    end do
end
