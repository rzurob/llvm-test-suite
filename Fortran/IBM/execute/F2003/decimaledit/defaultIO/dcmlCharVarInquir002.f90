!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/12/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the use of the substring for the variable
!                               to DECIMAL=.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dcmlCharVarInquir002
    character(:), allocatable :: mode(:)

    allocate (character(20) :: mode(3))

    mode = repeat(' ', 20)

    open (1, file = 'test1')
    open (2, file = 'test2', access='stream')

    open (3, file = 'test3', form='formatted', decimal='comma')

    read (1, *, decimal='COMMA', iostat=istat) r1

    if (istat == 0) stop 1

    read (3, '(g12.4)', decimal='POINT', iostat=istat) r2

    if (istat == 0) stop 2

    do i = 1, 3
        inquire(i, decimal=mode(i)(i:))
    end do

    if (mode(1) /= 'POINT') error stop 1_4

    if (mode(2) /= ' UNDEFINED') error stop 2_4

    if (mode(3) /= '  COMMA') error stop 3_4

    close(1, status='delete')
    close(2, status='delete')
    close(3, status='delete')
end
