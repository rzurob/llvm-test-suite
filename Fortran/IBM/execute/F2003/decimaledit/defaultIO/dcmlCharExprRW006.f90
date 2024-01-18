!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/08/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Write to the ERROR_UNIT using the decimal mode
!                               as COMMA.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dcmlCharExprRW006
use ISO_FORTRAN_ENV
    complex(8) :: cx(10)

    real (16), allocatable :: q1(:,:)

    allocate (q1(3, 4), source=reshape((/(i*1.0q3, i=1,20)/), (/3, 4/)))

    cx = (/(cmplx(i, i), i=1, 10)/)

    write (error_unit, *, decimal='COMMA') 'cx(::2) = ', cx(::2), &
        "with size of ", size(cx(::2))

    write (error_unit, '(a)', decimal='COMMA', advance='no') 'q1', &
            new_line('xz')

    write (error_unit, '("q1(", i1, ";", i1, "):", ES15.6)', decimal='COMMA') &
            ((i, j, q1(i,j), i=1,3), j=1,4)
end
