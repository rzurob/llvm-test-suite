!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/16/2006
!*
!*  DESCRIPTION                : DECIMLA EDIT MODE
!                               Test that the decimal edit descriptor takes
!                               effect during format processing (overrides
!                               others)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program decEditDesc001

    real(8) d1(10)
    complex(16) cx(5)

    d1 = (/(i,i=1,10)/)

    cx = cmplx(1.0, 1.0, 8)

    open (1, file = 'form1', access='stream', form='formatted')

    open (11, file='form2', access='sequential', decimal='cOmmA   ')

    write(1, '(g10.2, dc, f10.2, dp)', decimal='comma', pos=1, advance='no') d1
    write(1, '(e10.2, 10x, DC)') d1

    inquire (1, pos=ipos)

    write(1, '(5(es10.2, 10x, DC, EN10.2, DP))', pos=ipos) d1

    write (11, 100) d1
    write (11, 100, decimal='pOInt  ') d1
    write (11, 200, decimal='pOInt  ') (d1(2*i-1), cx(i), d1(2*i), i=1,5)

100 format (e10.3, DP, 2d10.3, DC, DP, dc, f10.2)
200 format (e10.3, DP, " (", 2d10.3, DC, ") ", g10.3, DP)
    end
