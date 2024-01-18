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
!*  are placed inside objects of type complex and kind 16.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11

      integer(4) :: inputno ! keeps track of the namelist input in the input file
      integer :: ios

      complex(16)   :: cx1, cx2, cx3, cx4, cx5, cx6, cx7, cx8

      real(16)      :: rl1r, rl1i, rl2r, rl2i, rl3r, rl3i, rl4r, rl4i, &
     &                 rl5r, rl5i, rl6r, rl6i, rl7r, rl7i, rl8r, rl8i

      real(8)       :: rl1re, rl1ie, rl2re, rl2ie, rl3re, rl3ie, rl4re,&
     &                 rl4ie, rl5re, rl5ie, rl6re, rl6ie, rl7re, rl7ie,&
     &                 rl8re, rl8ie

      integer(8)   :: ii1r, ii1i, ii2r, ii2i, ii3r, ii3i, ii4r, ii4i,  &
     &                ii5r, ii5i, ii6r, ii6i, ii7r, ii7i, ii8r, ii8i
      integer      :: i1
      character(3) :: c1

      logical precision_r6

      equivalence(rl1r, ii1r, rl1re); equivalence(rl1i, ii1i, rl1ie)
      equivalence(rl2r, ii2r, rl2re); equivalence(rl2i, ii2i, rl2ie)
      equivalence(rl3r, ii3r, rl3re); equivalence(rl3i, ii3i, rl3ie)
      equivalence(rl4r, ii4r, rl4re); equivalence(rl4i, ii4i, rl4ie)
      equivalence(rl5r, ii5r, rl5re); equivalence(rl5i, ii5i, rl5ie)
      equivalence(rl6r, ii6r, rl6re); equivalence(rl6i, ii6i, rl6ie)
      equivalence(rl7r, ii7r, rl7re); equivalence(rl7i, ii7i, rl7ie)
      equivalence(rl8r, ii8r, rl8re); equivalence(rl8i, ii8i, rl8ie)


      namelist /mynml/ cx1, cx2, c1, cx3, cx4, i1, cx5, cx6, cx7, cx8

      open(in, file='nmlInputComplex003.dat', action='read')

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

         rl1r = qreal(cx1)
         rl1i = qimag(cx1)

         ! check the real part of cx1: must be NaN, Signaling and Positive
         if ( ( .not. ieee_is_nan(rl1re) ) .or.                        &
     &        ( ieee_class(rl1re) .ne. ieee_signaling_nan ) .or.       &
     &        ( ii1r .le. 0 ) ) call zzrc( (inputno*100_4) + 1_4 )

         ! check the imaginary part of cx1: must be inf and negative
         if ( ieee_is_finite(rl1ie) .or. ( ii1i .ge. 0 ) )             &
     &        call zzrc( (inputno*100_4) + 2_4 )

         !*********************************************************
         !* Checking CX2
         !*********************************************************

         rl2r = qreal(cx2)
         rl2i = qimag(cx2)

         ! check the real part of cx2: must be Inf and positive
         if ( ieee_is_finite(rl2re) .or. ( ii2r .le. 0 ) )             &
     &        call zzrc( (inputno*100_4) + 3_4 )

         ! check imaginary part of cx2: must be NaN, quiet, and positive
         if ( ( .not. ieee_is_nan(rl2ie) ) .or.                        &
     &        ( ieee_class(rl2ie) .ne. ieee_quiet_nan ) .or.           &
     &        ( ii2i .le. 0 ) ) call zzrc( (inputno*100_4) + 4_4 )

         !*********************************************************
         !* Checking C1
         !*********************************************************

         if ( c1 .ne. 'xlf' ) call zzrc( (inputno*100_4) + 5_4 )

         !*********************************************************
         !* Checking CX3
         !*********************************************************

         rl3r = qreal(cx3)
         rl3i = qimag(cx3)

         ! check the real part of cx3: must be NaN, quiet, and negative
         if ( ( .not. ieee_is_nan(rl3re) ) .or.                        &
     &        ( ieee_class(rl3re) .ne. ieee_quiet_nan ) .or.           &
     &        ( ii3r .ge. 0 ) ) call zzrc( (inputno*100_4) + 6_4 )

         ! check imaginary part of cx3: must be NaN, signaling, and negative
         if ( ( .not. ieee_is_nan(rl3ie) ) .or.                        &
     &        ( ieee_class(rl3ie) .ne. ieee_signaling_nan ) .or.       &
     &        ( ii3i .ge. 0 ) ) call zzrc( (inputno*100_4) + 7_4 )

         !*********************************************************
         !* Checking CX4
         !*********************************************************

         rl4r = qreal(cx4)
         rl4i = qimag(cx4)

         ! check the real part of cx4: must be infinity and positive
         if ( ieee_is_finite(rl4re) .or. ( ii4r .le. 0 ) )             &
     &        call zzrc( (inputno*100_4) + 8_4 )

         ! check the imaginary part of cx4: must be infinity and positive
         if ( ieee_is_finite(rl4ie) .or. ( ii4i .le. 0 ) )             &
     &        call zzrc( (inputno*100_4) + 9_4 )

         !*********************************************************
         !* Checking I1
         !*********************************************************

         if ( i1 .ne. 11 ) call zzrc( (inputno*100_4) + 10_4 )

         !*********************************************************
         !* Checking CX5
         !*********************************************************

         rl5r = qreal(cx5)
         rl5i = qimag(cx5)

         ! check the real part of cx5: must be NaN, signaling, and negative
         if ( ( .not. ieee_is_nan(rl5re) ) .or.                        &
     &        ( ieee_class(rl5re) .ne. ieee_signaling_nan ) .or.       &
     &        ( ii5r .ge. 0 ) ) call zzrc( (inputno*100_4) + 11_4 )

         ! check the imaginary part of cx5: must be 3.14
         if ( .not. precision_r6(3.14_16, rl5i) )                      &
     &        call zzrc( (inputno*100_4) + 12_4 )

         !*********************************************************
         !* Checking CX6
         !*********************************************************

         rl6r = qreal(cx6)
         rl6i = qimag(cx6)

         ! check the real part of cx6: must be 3.14
         if ( .not. precision_r6(-3.14_16, rl6r) )                     &
     &        call zzrc( (inputno*100_4) + 13_4 )

         ! check the imaginary part of cx6: must be Inf and negative
         if ( ieee_is_finite(rl6ie) .or. ( ii6i .ge. 0 ) )             &
     &        call zzrc( (inputno*100_4) + 14_4 )

         !*********************************************************
         !* Checking CX7
         !*********************************************************

         rl7r = qreal(cx7)
         rl7i = qimag(cx7)

         ! check the real part of cx7: must be Inf and negative
         if ( ieee_is_finite(rl7re) .or. ( ii7r .ge. 0 ) )             &
     &        call zzrc( (inputno*100_4) + 15_4 )

         ! check the imaginary part of cx7: must be Inf and negative
         if ( ieee_is_finite(rl7ie) .or. ( ii7i .ge. 0 ) )             &
     &        call zzrc( (inputno*100_4) + 16_4 )

         !*********************************************************
         !* Checking CX8
         !*********************************************************

         rl8r = qreal(cx8)
         rl8i = qimag(cx8)

         ! check the real part of cx8: must be NaN, signaling, and positive
         if ( ( .not. ieee_is_nan(rl8re) ) .or.                        &
     &        ( ieee_class(rl8re) .ne. ieee_signaling_nan ) .or.       &
     &        ( ii8r .le. 0 ) ) call zzrc( (inputno*100_4) + 17_4 )

         ! check the imaginary part of cx8: must be NaN, signaling, and positive
         if ( ( .not. ieee_is_nan(rl8ie) ) .or.                        &
     &        ( ieee_class(rl8ie) .ne. ieee_signaling_nan ) .or.       &
     &        ( ii8i .le. 0 ) ) call zzrc( (inputno*100_4) + 18_4 )

      end do

      if ( inputno .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      close(in)

      end
