!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : signOfNaNInput001.f
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
!*  NaN values. Testing REAL(4)s.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none
      
      real(4)  :: rl1
      integer(4) :: i1

      integer, parameter :: in = 11

      equivalence(i1, rl1)

      open(in, file='signOfNaNInput001.dat', action='read')

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 1_4

      rl1 = 0.0
      read(in, '(f10.2)') rl1
      if ( i1 .ge. 0  ) error stop 2_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 3_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 4_4

      rl1 = 0.0
      read(in, '(f10.2)') rl1
      if ( i1 .ge. 0  ) error stop 5_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 6_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 7_4

      rl1 = 0.0
      read(in, '(f10.2)') rl1
      if ( i1 .ge. 0  ) error stop 8_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 9_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 10_4

      rl1 = 0.0
      read(in, '(f10.2)') rl1
      if ( i1 .ge. 0  ) error stop 11_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 12_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 13_4

      rl1 = 0.0
      read(in, '(f10.2)') rl1
      if ( i1 .ge. 0  ) error stop 14_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 15_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 16_4

      rl1 = 0.0
      read(in, '(f10.2)') rl1
      if ( i1 .ge. 0  ) error stop 17_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 18_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 19_4

      rl1 = 0.0
      read(in, '(f10.2)') rl1
      if ( i1 .ge. 0  ) error stop 20_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 21_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 22_4

      rl1 = 0.0
      read(in, '(f10.2)') rl1
      if ( i1 .ge. 0  ) error stop 23_4

      rl1 = -1.0
      read(in, '(f10.2)') rl1
      if ( i1 .le. 0  ) error stop 24_4

      close(in)
      
      end
