! *********************************************************************
! cinterop/pattr/fxbind_c03eed.f, xlftest.cinterop, tstdev.cinterop, 1.1
! Extract Date/Time: 04/04/06 11:11:56
! Checkin Date/Time: 04/03/23 17:32:51
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/runcomplex.sh fxbind_c03eed  cxbind_c03eed
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran Entry in function called from C
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with  different intrinsic data type,
!*           complex*4,complex*8.
!*   - The interoperable  procedure itself is implemented using Fortran
!*     function Entry Statement.
!*   - Both primary entry point and an alternate entry point
!*     have bind(c) attribute.
!*   - passing scalar arguments by REFERENCE and by VALUE
!*   - main written in C, C calls FORTRAN functions.
!*
!*  ALGORITHM :
!*          1. C program call the Fortran function has a primary entry
!*             point and an alternate entry point.
!*          2. Assertion: Check the return value in C
!*             to verify it is correct.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

FUNCTION swap_c4(a,b) bind(c)
  COMPLEX(4), INTENT(INOUT) :: a,b
  COMPLEX(4) ::swap_c4, dum
  real::enswap_c4
  dum=a
  a=b
  b=dum
  swap_c4=a
  return
  entry enswap_c4 (a,b) bind(c)
  enswap_c4 = REAL(a)
  return
END FUNCTION swap_c4

FUNCTION swap_c8(a,b) bind(c)
  COMPLEX(8), INTENT(INOUT) :: a,b
  COMPLEX(8) ::swap_c8, dum
  real::enswap_c8
  dum=a
  a=b
  b=dum
  swap_c8=a
  return
  entry enswap_c8 (a,b) bind(c)
  enswap_c8 = REAL(a)
  return
END FUNCTION swap_c8

FUNCTION swapval_c4(a,b) bind(c)
  COMPLEX(4),value :: a,b
  COMPLEX(4)::swapval_c4, dum
  real::enswapval_c4
  dum=a
  a=b
  b=dum
  swapval_c4=a
  return
  entry enswapval_c4 (a,b) bind(c)
  enswapval_c4 = REAL(a)
  return
END FUNCTION swapval_c4

FUNCTION swapval_c8(a,b) bind(c)
  COMPLEX(8),value  :: a,b
  COMPLEX(8) ::swapval_c8, dum
  real::enswapval_c8
  dum=a
  a=b
  b=dum
  swapval_c8=a
  return
  entry enswapval_c8 (a,b) bind(c)
  enswapval_c8 = REAL(a)
  return
END FUNCTION swapval_c8

