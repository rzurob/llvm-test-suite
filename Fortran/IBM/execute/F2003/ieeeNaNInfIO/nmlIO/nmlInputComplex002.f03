!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 22, 2006
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
!*  Test input of IEEE NaN and Inf for namelist I/O.
!*  In this testcase IEEE exceptional specifications
!*  are placed inside objects of type complex and kind 8.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11

      integer(4) :: inputno ! keeps track of the namelist input in the input file
      integer :: ios

      complex(8)   :: cx1, cx2, cx3, cx4, cx5, cx6, cx7, cx8
      real(8)      :: rl1r, rl1i, rl2r, rl2i, rl3r, rl3i, rl4r, rl4i,  &
     &                rl5r, rl5i, rl6r, rl6i, rl7r, rl7i, rl8r, rl8i
      integer(8)   :: ii1r, ii1i, ii2r, ii2i, ii3r, ii3i, ii4r, ii4i,  &
     &                ii5r, ii5i, ii6r, ii6i, ii7r, ii7i, ii8r, ii8i
      integer      :: i1
      character(3) :: c1

      logical precision_r8

      equivalence(rl1r, ii1r); equivalence(rl1i, ii1i)
      equivalence(rl2r, ii2r); equivalence(rl2i, ii2i)
      equivalence(rl3r, ii3r); equivalence(rl3i, ii3i)
      equivalence(rl4r, ii4r); equivalence(rl4i, ii4i)
      equivalence(rl5r, ii5r); equivalence(rl5i, ii5i)
      equivalence(rl6r, ii6r); equivalence(rl6i, ii6i)
      equivalence(rl7r, ii7r); equivalence(rl7i, ii7i)
      equivalence(rl8r, ii8r); equivalence(rl8i, ii8i)


      namelist /mynml/ cx1, cx2, c1, cx3, cx4, i1, cx5, cx6, cx7, cx8

      open(in, file='nmlInputComplex002.dat', action='read')

      inputno = 0

      do
         inputno = inputno + 1

         ! initialize to zeros before reading each input item
         cx1 = (0.0, 0.0); cx2 = (0.0, 0.0); cx3 = (0.0, 0.0)
         cx4 = (0.0, 0.0); cx5 = (0.0, 0.0); cx6 = (0.0, 0.0)
         cx7 = (0.0, 0.0); cx8 = (0.0, 0.0);

         rl1i = 0.0; rl2i = 0.0; rl3i = 0.0; rl4i = 0.0;
         rl5i = 0.0; rl6i = 0.0; rl7i = 0.0; rl8i = 0.0;

         rl1r = 0.0; rl2r = 0.0; rl3r = 0.0; rl4r = 0.0;
         rl5r = 0.0; rl6r = 0.0; rl7r = 0.0; rl8r = 0.0;

         i1 = 0; c1 = 'xxx'

         read(in,nml=mynml,iostat=ios)

         if ( is_iostat_end(ios) ) exit

         ! check the values read in one-by-one. The 2 least significant
         ! digits of the return value indicate which item in the
         ! input list caused the error. The most significant digits
         ! represent the input count number.

         !*********************************************************
         !* Checking CX1
         !*********************************************************

         rl1r = dreal(cx1)
         rl1i = dimag(cx1)

         ! check the real part of cx1: must be NaN, Signaling and Positive
         if ( ( .not. ieee_is_nan(rl1r) ) .or.                         &
     &        ( ieee_class(rl1r) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii1r .le. 0 ) ) call zzrc( (inputno*100_4) + 1_4 )

         ! check the imaginary part of cx1: must be inf and negative
         if ( ieee_is_finite(rl1i) .or. ( ii1i .ge. 0 ) )              &
     &        call zzrc( (inputno*100_4) + 2_4 )

         !*********************************************************
         !* Checking CX2
         !*********************************************************

         rl2r = dreal(cx2)
         rl2i = dimag(cx2)

         ! check the real part of cx2: must be Inf and positive
         if ( ieee_is_finite(rl2r) .or. ( ii2r .le. 0 ) )              &
     &        call zzrc( (inputno*100_4) + 3_4 )

         ! check imaginary part of cx2: must be NaN, quiet, and positive
         if ( ( .not. ieee_is_nan(rl2i) ) .or.                         &
     &        ( ieee_class(rl2i) .ne. ieee_quiet_nan ) .or.            &
     &        ( ii2i .le. 0 ) ) call zzrc( (inputno*100_4) + 4_4 )

         !*********************************************************
         !* Checking C1
         !*********************************************************

         if ( c1 .ne. 'xlf' ) call zzrc( (inputno*100_4) + 5_4 )

         !*********************************************************
         !* Checking CX3
         !*********************************************************

         rl3r = dreal(cx3)
         rl3i = dimag(cx3)

         ! check the real part of cx3: must be NaN, quiet, and negative
         if ( ( .not. ieee_is_nan(rl3r) ) .or.                         &
     &        ( ieee_class(rl3r) .ne. ieee_quiet_nan ) .or.            &
     &        ( ii3r .ge. 0 ) ) call zzrc( (inputno*100_4) + 6_4 )

         ! check imaginary part of cx3: must be NaN, signaling, and negative
         if ( ( .not. ieee_is_nan(rl3i) ) .or.                         &
     &        ( ieee_class(rl3i) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii3i .ge. 0 ) ) call zzrc( (inputno*100_4) + 7_4 )

         !*********************************************************
         !* Checking CX4
         !*********************************************************

         rl4r = dreal(cx4)
         rl4i = dimag(cx4)

         ! check the real part of cx4: must be infinity and positive
         if ( ieee_is_finite(rl4r) .or. ( ii4r .le. 0 ) )              &
     &        call zzrc( (inputno*100_4) + 8_4 )

         ! check the imaginary part of cx4: must be infinity and positive
         if ( ieee_is_finite(rl4i) .or. ( ii4i .le. 0 ) )              &
     &        call zzrc( (inputno*100_4) + 9_4 )

         !*********************************************************
         !* Checking I1
         !*********************************************************

         if ( i1 .ne. 11 ) call zzrc( (inputno*100_4) + 10_4 )

         !*********************************************************
         !* Checking CX5
         !*********************************************************

         rl5r = dreal(cx5)
         rl5i = dimag(cx5)

         ! check the real part of cx5: must be NaN, signaling, and negative
         if ( ( .not. ieee_is_nan(rl5r) ) .or.                         &
     &        ( ieee_class(rl5r) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii5r .ge. 0 ) ) call zzrc( (inputno*100_4) + 11_4 )

         ! check the imaginary part of cx5: must be 3.14
         if ( .not. precision_r8(3.14_8, rl5i) )                         &
     &        call zzrc( (inputno*100_4) + 12_4 )

         !*********************************************************
         !* Checking CX6
         !*********************************************************

         rl6r = dreal(cx6)
         rl6i = dimag(cx6)

         ! check the real part of cx6: must be 3.14
         if ( .not. precision_r8(-3.14_8, rl6r) )                        &
     &        call zzrc( (inputno*100_4) + 13_4 )

         ! check the imaginary part of cx6: must be Inf and negative
         if ( ieee_is_finite(rl6i) .or. ( ii6i .ge. 0 ) )              &
     &        call zzrc( (inputno*100_4) + 14_4 )

         !*********************************************************
         !* Checking CX7
         !*********************************************************

         rl7r = dreal(cx7)
         rl7i = dimag(cx7)

         ! check the real part of cx7: must be Inf and negative
         if ( ieee_is_finite(rl7r) .or. ( ii7r .ge. 0 ) )              &
     &        call zzrc( (inputno*100_4) + 15_4 )

         ! check the imaginary part of cx7: must be Inf and negative
         if ( ieee_is_finite(rl7i) .or. ( ii7i .ge. 0 ) )              &
     &        call zzrc( (inputno*100_4) + 16_4 )

         !*********************************************************
         !* Checking CX8
         !*********************************************************

         rl8r = dreal(cx8)
         rl8i = dimag(cx8)

         ! check the real part of cx8: must be NaN, signaling, and positive
         if ( ( .not. ieee_is_nan(rl8r) ) .or.                         &
     &        ( ieee_class(rl8r) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii8r .le. 0 ) ) call zzrc( (inputno*100_4) + 17_4 )

         ! check the imaginary part of cx8: must be NaN, signaling, and positive
         if ( ( .not. ieee_is_nan(rl8i) ) .or.                         &
     &        ( ieee_class(rl8i) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii8i .le. 0 ) ) call zzrc( (inputno*100_4) + 18_4 )

      end do

      if ( inputno .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      close(in)

      end