!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 23, 2006
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
!*  Test input of IEEE NaN and Inf for list-directed I/O.
!*  In this testcase IEEE exceptional specifications
!*  are placed inside objects of type COMPLEX and kind 16.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11

      complex(16)   :: cx1, cx2, cx3, cx4, cx5, cx6, cx7, cx8, cx9, cx10

      ! 16-bytes imaginary part of each complex number
      real(16)      :: rl1i, rl2i, rl3i, rl4i, rl5i, rl6i, rl7i, rl8i, &
     &                 rl9i, rl10i

      ! 16-bytes real part of each complex number
      real(16)      :: rl1r, rl2r, rl3r, rl4r, rl5r, rl6r, rl7r, rl8r, &
     &                 rl9r, rl10r

      ! 8-bytes equivalence of the imaginary part of each complex number
      real(8)       :: rl1ie, rl2ie, rl3ie, rl4ie, rl5ie, rl6ie, rl7ie,&
     &                 rl8ie, rl9ie, rl10ie

      ! 8-bytes equivalence of the real part of each complex number
      real(8)       :: rl1re, rl2re, rl3re, rl4re, rl5re, rl6re, rl7re,&
     &                 rl8re, rl9re, rl10re

      ! integer equivalence of imaginary part of each complex number
      integer(8)   :: ii1i, ii2i, ii3i, ii4i, ii5i, ii6i, ii7i, ii8i,  &
     &                ii9i, ii10i

      ! integer equivalence of real part of each complex number
      integer(8)   :: ii1r, ii2r, ii3r, ii4r, ii5r, ii6r, ii7r, ii8r,  &
     &                ii9r, ii10r
      integer      :: i1
      character(3) :: c1

      integer(4) :: ios = 0, count = 0

      logical precision_r8

      equivalence(rl1i, ii1i, rl1ie); equivalence(rl1r, ii1r, rl1re)
      equivalence(rl2i, ii2i, rl2ie); equivalence(rl2r, ii2r, rl2re)
      equivalence(rl3i, ii3i, rl3ie); equivalence(rl3r, ii3r, rl3re)
      equivalence(rl4i, ii4i, rl4ie); equivalence(rl4r, ii4r, rl4re)
      equivalence(rl5i, ii5i, rl5ie); equivalence(rl5r, ii5r, rl5re)
      equivalence(rl6i, ii6i, rl6ie); equivalence(rl6r, ii6r, rl6re)
      equivalence(rl7i, ii7i, rl7ie); equivalence(rl7r, ii7r, rl7re)
      equivalence(rl8i, ii8i, rl8ie); equivalence(rl8r, ii8r, rl8re)
      equivalence(rl9i, ii9i, rl9ie); equivalence(rl9r, ii9r, rl9re)
      equivalence(rl10i, ii10i, rl10ie)
      equivalence(rl10r, ii10r, rl10re)


      open(in, file='lstInputComplex003.dat', action='read')

      do
         count = count + 1

         ! reset variables
         cx1 = (0.0, 0.0); cx2 = (0.0, 0.0); cx3 = (0.0, 0.0)
         cx4 = (0.0, 0.0); cx5 = (0.0, 0.0); cx6 = (0.0, 0.0)
         cx7 = (0.0, 0.0); cx8 = (0.0, 0.0); cx9 = (0.0, 0.0)
         cx10 = (0.0, 0.0)
         i1 = 0
         c1 = 'xxx'

         ! read in the values
         read(in,*, iostat=ios)                                       &
     &        cx1, cx2, cx3, cx4, i1, cx5, cx6, c1, cx7, cx8, cx9, cx10

         if ( is_iostat_end(ios) ) exit ! end of file - leave the loop

         ! validate the values read into the variables one by one
         ! The 2 least significant digits of the return value indicate
         ! which item in the input list caused the error.
         ! The most significant digits represent the input count number.


         ! checking cx1: (+inf, +inf)
         rl1r = qreal(cx1)
         rl1i = qimag(cx1)

         if( ieee_is_finite(rl1re) .or. ( ii1r .le. 0 ) )              &
     &        call zzrc( (count*100_4) + 1_4 )

         if( ieee_is_finite(rl1ie) .or. ( ii1i .le. 0 ) )              &
     &        call zzrc( (count*100_4) + 2_4 )


         ! checking cx2: (-inf, -inf)
         rl2r = qreal(cx2)
         rl2i = qimag(cx2)

         if( ieee_is_finite(rl2re) .or. ( ii2r .ge. 0 ) )              &
     &        call zzrc( (count*100_4) + 3_4 )

         if( ieee_is_finite(rl2ie) .or. ( ii2i .ge. 0 ) )              &
     &        call zzrc( (count*100_4) + 4_4 )


         ! checking cx3: (-inf, +inf)
         rl3r = qreal(cx3)
         rl3i = qimag(cx3)

         if( ieee_is_finite(rl3re) .or. ( ii3r .ge. 0 ) )              &
     &        call zzrc( (count*100_4) + 5_4 )

         if( ieee_is_finite(rl3ie) .or. ( ii3i .le. 0 ) )              &
     &        call zzrc( (count*100_4) + 6_4 )


         ! checking cx4: (+nan(s), +nan(q))
         rl4r = qreal(cx4)
         rl4i = qimag(cx4)

         if ( ( .not. ieee_is_nan(rl4re) ) .or.                        &
     &        ( ieee_class(rl4re) .ne. ieee_signaling_nan ) .or.       &
     &        ( ii4r .le. 0 ) ) call zzrc( (count*100_4) + 7_4 )

         if ( ( .not. ieee_is_nan(rl4ie) ) .or.                        &
     &        ( ieee_class(rl4ie) .ne. ieee_quiet_nan ) .or.           &
     &        ( ii4i .le. 0 ) ) call zzrc( (count*100_4) + 8_4 )

         ! checking i1 : 123
         if ( i1 .ne. 123 ) call zzrc( (count*100_4) + 9_4 )

         ! checking cx5: (+nan(q), -nan(s))
         rl5r = qreal(cx5)
         rl5i = qimag(cx5)

         if ( ( .not. ieee_is_nan(rl5re) ) .or.                        &
     &        ( ieee_class(rl5re) .ne. ieee_quiet_nan ) .or.           &
     &        ( ii5r .le. 0 ) ) call zzrc( (count*100_4) + 10_4 )

         if ( ( .not. ieee_is_nan(rl5ie) ) .or.                        &
     &        ( ieee_class(rl5ie) .ne. ieee_signaling_nan ) .or.       &
     &        ( ii5i .ge. 0 ) ) call zzrc( (count*100_4) + 11_4 )


         ! checking cx6: (-nan(s), nan(q))
         rl6r = qreal(cx6)
         rl6i = qimag(cx6)

         if ( ( .not. ieee_is_nan(rl6re) ) .or.                        &
     &        ( ieee_class(rl6re) .ne. ieee_signaling_nan ) .or.       &
     &        ( ii6r .ge. 0 ) ) call zzrc( (count*100_4) + 12_4 )

         if ( ( .not. ieee_is_nan(rl6ie) ) .or.                        &
     &        ( ieee_class(rl6ie) .ne. ieee_quiet_nan ) .or.           &
     &        ( ii6i .le. 0 ) ) call zzrc( (count*100_4) + 13_4 )


         ! checking c1 : 'iBm'
         if ( c1 .ne. 'iBm' ) call zzrc( (count*100_4) + 14_4 )

         ! checking cx7: (+inf, -nan(s))
         rl7r = qreal(cx7)
         rl7i = qimag(cx7)

         if ( ieee_is_finite(rl7re) .or. ( ii7r .le. 0 ) )             &
     &        call zzrc( (count*100_4) + 15_4 )

         if ( ( .not. ieee_is_nan(rl7ie) ) .or.                        &
     &        ( ieee_class(rl7ie) .ne. ieee_signaling_nan ) .or.       &
     &        ( ii7i .ge. 0 ) ) call zzrc( (count*100_4) + 16_4 )


         ! checking cx8: (-inf, -nan(q))
         rl8r = qreal(cx8)
         rl8i = qimag(cx8)

         if ( ieee_is_finite(rl8re) .or. ( ii8r .ge. 0 ) )             &
     &        call zzrc( (count*100_4) + 17_4 )

         if ( ( .not. ieee_is_nan(rl8ie) ) .or.                        &
     &        ( ieee_class(rl8ie) .ne. ieee_quiet_nan ) .or.           &
     &        ( ii8i .ge. 0 ) ) call zzrc( (count*100_4) + 18_4 )


         ! checking cx9: (-2.0, -nan(s))
         rl9r = qreal(cx9)
         rl9i = qimag(cx9)

         if ( .not. precision_r8(rl9re, -2.0_8) )                      &
     &        call zzrc( (count*100_4) + 19_4 )

         if ( ( .not. ieee_is_nan(rl9ie) ) .or.                        &
     &        ( ieee_class(rl9ie) .ne. ieee_signaling_nan ) .or.       &
     &        ( ii9i .ge. 0 ) ) call zzrc( (count*100_4) + 20_4 )


         ! checking cx10: (-inf, +3.0)
         rl10r = qreal(cx10)
         rl10i = qimag(cx10)

         if ( ieee_is_finite(rl10re) .or. ( ii10r .ge. 0 ) )           &
     &        call zzrc( (count*100_4) + 21_4 )

         if ( .not. precision_r8(rl10ie, 3.0_8) )                      &
     &        call zzrc( (count*100_4) + 22_4 )

      end do

      if ( count .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      close(in)

      end
