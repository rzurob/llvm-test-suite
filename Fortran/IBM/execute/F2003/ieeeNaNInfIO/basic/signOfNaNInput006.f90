!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : signOfNaNInput006.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 20, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Test to make sure the sign bit is preserved when doing input of
!*  NaN values. Testing COMPLEX(16).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none
      
      complex(16) :: cx1
      real(16)    :: rlr, rli
      integer(8) :: ir, ii

      integer, parameter :: in = 11

      equivalence(rlr, ir)
      equivalence(rli, ii)

      open(in, file='signOfNaNInput006.dat', action='read')

      read(in, '(2f4.2)') cx1
      rlr = qreal(cx1)
      rli = qimag(cx1)
      if ( ir .le. 0  ) error stop 1_4
      if ( ii .le. 0  ) error stop 2_4

      read(in, '(2f4.2)') cx1
      rlr = qreal(cx1)
      rli = qimag(cx1)
      if ( ir .le. 0  ) error stop 3_4
      if ( ii .le. 0  ) error stop 4_4

      read(in, '(2f4.2)') cx1
      rlr = qreal(cx1)
      rli = qimag(cx1)
      if ( ir .le. 0  ) error stop 5_4
      if ( ii .ge. 0  ) error stop 6_4

      read(in, '(f4.2, f6.3)') cx1
      rlr = qreal(cx1)
      rli = qimag(cx1)
      if ( ir .ge. 0  ) error stop 7_4
      if ( ii .le. 0  ) error stop 8_4

      read(in, '(f6.2, f7.3)') cx1
      rlr = qreal(cx1)
      rli = qimag(cx1)
      if ( ir .le. 0  ) error stop 9_4
      if ( ii .le. 0  ) error stop 10_4

      read(in, '(f7.0, f6.3)') cx1
      rlr = qreal(cx1)
      rli = qimag(cx1)
      if ( ir .le. 0  ) error stop 11_4
      if ( ii .le. 0  ) error stop 12_4

      read(in, '(f6.2, f8.0)') cx1
      rlr = qreal(cx1)
      rli = qimag(cx1)
      if ( ir .le. 0  ) error stop 13_4
      if ( ii .ge. 0  ) error stop 14_4

      read(in, '(f4.2, f8.3)') cx1
      rlr = qreal(cx1)
      rli = qimag(cx1)
      if ( ir .ge. 0  ) error stop 15_4
      if ( ii .le. 0  ) error stop 16_4

      close(in)
      
      end
