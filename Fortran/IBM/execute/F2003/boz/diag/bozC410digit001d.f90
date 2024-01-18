!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : bozC410digit001d.f
!*
!*  PROGRAMMER                 : Vicram Uppal
!*  DATE                       : 01/19/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.4.1: boz-literal-constant
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*				
!*
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                :
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program bozC410digit001d

    integer :: i1, i2, i3
    real :: r1, r2, r3
    complex :: c1, c2, c3
    real(4) :: d1, d3, d4

    i1 = int(1,KIND=(b'1'+b'1'))
    i2 = int(2,KIND=o"4")
    i3 = int(3,KIND=z'8')

    r1 = real(4,KIND=B"100")
    r2 = real(5,KIND=O'10')
    r3 = real(6,KIND=Z"1"+z'1')

    c1 = cmplx(7,8,KIND=B'1000')
    c2 = cmplx(9,10,KIND=(o"1"+O'1'))
    c3 = cmplx(11,12,KIND=Z'4')

    d1 = dble(B'10000'+b"11")
    d2 = dble(O"671"+o'12')
    d3 = dble(Z'CD9'+Z"F12")

  end
