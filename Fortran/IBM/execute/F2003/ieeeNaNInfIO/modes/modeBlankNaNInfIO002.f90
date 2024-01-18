!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : modeBlankNaNInfIO002.f
!*
!*  DATE                       : June 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Testing list-directed input of NaN and Inf with different BLANK
!*  modes. Also make sure the output is not affected.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: in = 11, out = 12

      real(4)    :: rl4, rl4r, rl4i
      real(8)    :: rl8, rl8r, rl8i
      complex(4) :: cx4
      complex(8) :: cx8
      integer(4) :: ii4, ii_real4, ii_imag4
      integer(8) :: ii8, ii_real8, ii_imag8

      equivalence(rl4, ii4)
      equivalence(rl4r, ii_real4)
      equivalence(rl4i, ii_imag4)
      equivalence(rl8, ii8)
      equivalence(rl8r, ii_real8)
      equivalence(rl8i, ii_imag8)

      open(in, file='modeBlankNaNInfIO002.dat', blank='zero')
      open(out, file='modeBlankNaNInfIO002.out', blank='zero')

      !*******************************************************
      !* READ THE FIRST TWO LINES OF INPUT IN BLANK='ZERO'
      !* AND BLANK='NULL' FORMS.
      !*******************************************************

      ! reset variables
      rl4 = 0.0; rl8 = 0.0; cx4 = (0.0,0.0); cx8 = (0.0,0.0)

      ! read the values in using BLANK mode of ZERO
      read(in, *, blank='zero')  rl4, rl8, cx4, cx8

      rl4r = real(cx4)
      rl4i = imag(cx4)
      rl8r = dreal(cx8)
      rl8i = dimag(cx8)

      ! check the signs
      if ( ( ii4 .ge. 0 ) .or. ( ii8 .ge. 0 ) .or.                     &
     &     ( ii_real4 .le. 0 ) .or. ( ii_imag4 .le. 0 ) .or.           &
     &     ( ii_real8 .le. 0 ) .or. ( ii_imag8 .le. 0 ) ) error stop 1_4

      ! write the values out for verification
      write(out, *) rl4, rl8, cx4, cx8


      ! reset variables
      rl4 = 0.0; rl8 = 0.0; cx4 = (0.0,0.0); cx8 = (0.0,0.0)

      ! read the values in using BLANK mode of NULL
      read(in, *, blank='null')  rl4, rl8, cx4, cx8

      rl4r = real(cx4)
      rl4i = imag(cx4)
      rl8r = dreal(cx8)
      rl8i = dimag(cx8)

      ! check the signs
      if ( ( ii4 .ge. 0 ) .or. ( ii8 .ge. 0 ) .or.                     &
     &     ( ii_real4 .le. 0 ) .or. ( ii_imag4 .le. 0 ) .or.           &
     &     ( ii_real8 .le. 0 ) .or. ( ii_imag8 .le. 0 ) ) error stop 2_4

      ! write the values out for verification
      write(out, *) rl4, rl8, cx4, cx8


      !*******************************************************
      !* READ THE NEXT TWO LINES OF INPUT IN BLANK='ZERO'
      !* AND BLANK='NULL' FORMS.
      !*******************************************************

      ! reset variables
      rl4 = 0.0; rl8 = 0.0; cx4 = (0.0,0.0); cx8 = (0.0,0.0)

      ! read the values in using BLANK mode of ZERO
      read(in, *, blank='zero')  rl4, rl8, cx4, cx8

      rl4r = real(cx4)
      rl4i = imag(cx4)
      rl8r = dreal(cx8)
      rl8i = dimag(cx8)

      ! check the signs
      if ( ( ii4 .le. 0 ) .or. ( ii8 .le. 0 ) .or.                     &
     &     ( ii_real4 .ge. 0 ) .or. ( ii_imag4 .ge. 0 ) .or.           &
     &     ( ii_real8 .ge. 0 ) .or. ( ii_imag8 .le. 0 ) ) error stop 3_4

      ! write the values out for verification
      write(out, *) rl4, rl8, cx4, cx8

      ! reset variables
      rl4 = 0.0; rl8 = 0.0; cx4 = (0.0,0.0); cx8 = (0.0,0.0)

      ! read the values in using BLANK mode of NULL
      read(in, *, blank='null')  rl4, rl8, cx4, cx8

      rl4r = real(cx4)
      rl4i = imag(cx4)
      rl8r = dreal(cx8)
      rl8i = dimag(cx8)

      ! check the signs
      if ( ( ii4 .le. 0 ) .or. ( ii8 .le. 0 ) .or.                     &
     &     ( ii_real4 .ge. 0 ) .or. ( ii_imag4 .ge. 0 ) .or.           &
     &     ( ii_real8 .ge. 0 ) .or. ( ii_imag8 .le. 0 ) ) error stop 4_4

      ! write the values out for verification
      write(out, *) rl4, rl8, cx4, cx8


      close(in)
      close(out)

      end
