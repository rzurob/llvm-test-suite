!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ/WRITE statement
!*
!*  DESCRIPTION                :
!*          diagnostic testcase for ROUND specifier can't be used in
!*          unformated I/O.
!* ===================================================================

  program roundRealRWEdit01d

    implicit none

    real*8 rd1, rd2, rd3, rd4

    integer, parameter::unit_w = 2

    rd1 = 1.2500509003215D0
    rd2 = -1.2500509003215D0

    open(unit_w, access="direct", recl=20, status="scratch", round="down")

    write(unit_w, rec=1, round="up") rd1, rd2

    read(unit_w, rec=1, round="processor_defined") rd3, rd4

    close(unit_w)

  end program roundRealRWEdit01d
