!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/23/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Change of the decimal mode during the same READ
!                               statement; e.g. how is field of "1.2,1,2;1.2"
!                               treated if format control is "(DP, F5.1,DC,
!                               F5.1, DP, F5.1)".
!                               This involves IBM extension.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program valueSeparator004
    real(4), allocatable :: r1(:,:)
    character(:), pointer :: c(:)

    logical(4), external :: precision_r4

    allocate (character(15) :: c(2))
    allocate (r1(2, 3))

    c = "1.2, 1,2; 1.2"

    read (c(1), '(dp, F5.1, DC, F5.1, DP, F5.1)', decimal='Comma') r1(1,:)

    write (1, '(a)', decimal='comma') c(2)

    rewind 1

    read (1, '(dp, F5.1, DC, F5.1, DP, F5.1)') r1(2,:)

    do i = 1, 2
        do j = 1, 3
            if (.not. precision_r4(r1(i,j), 1.2e0)) error stop 1_4
        end do
    end do
end
