! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/02/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 320300)
!                               use of deferred character type as the decimal
!                               edit mode.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(:), allocatable :: symbol
    character(:), pointer :: syms(:,:)

    allocate (symbol, source= 'COMMA')

    allocate (syms(2,2), source=reshape((/' POINT    ', '  COMMA   ',&
            '  COMMA   ', '   POINT  '/), (/2,2/)))

    write (*, '(f10.2)', decimal=symbol) 1.2, 2.4, 3.6

    do i = 1, 2
        do j = 1, 2
            write (*, '(2en10.2)', decimal=syms(i,j)(i+j:i+j+5)) cmplx(i,j)
        end do
    end do

    end
