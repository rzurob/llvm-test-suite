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
!*  are placed inside objects of type COMPLEX and kind 4.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11

      complex(4)   :: cx1, cx2, cx3, cx4, cx5, cx6, cx7, cx8, cx9, cx10
      real(4)      :: rl1i, rl2i, rl3i, rl4i, rl5i, rl6i, rl7i, rl8i,  &
     &                rl9i, rl10i
      real(4)      :: rl1r, rl2r, rl3r, rl4r, rl5r, rl6r, rl7r, rl8r,  &
     &                rl9r, rl10r
      integer(4)   :: ii1i, ii2i, ii3i, ii4i, ii5i, ii6i, ii7i, ii8i,  &
     &                ii9i, ii10i
      integer(4)   :: ii1r, ii2r, ii3r, ii4r, ii5r, ii6r, ii7r, ii8r,  &
     &                ii9r, ii10r
      integer      :: i1
      character(3) :: c1

      integer(4) :: ios = 0, count = 0

      logical precision_r4

      equivalence(rl1i, ii1i); equivalence(rl1r, ii1r)
      equivalence(rl2i, ii2i); equivalence(rl2r, ii2r)
      equivalence(rl3i, ii3i); equivalence(rl3r, ii3r)
      equivalence(rl4i, ii4i); equivalence(rl4r, ii4r)
      equivalence(rl5i, ii5i); equivalence(rl5r, ii5r)
      equivalence(rl6i, ii6i); equivalence(rl6r, ii6r)
      equivalence(rl7i, ii7i); equivalence(rl7r, ii7r)
      equivalence(rl8i, ii8i); equivalence(rl8r, ii8r)
      equivalence(rl9i, ii9i); equivalence(rl9r, ii9r)
      equivalence(rl10i, ii10i); equivalence(rl10r, ii10r)


      open(in, file='lstInputComplex001.dat', action='read')

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
         rl1r = real(cx1)
         rl1i = imag(cx1)

         if( ieee_is_finite(rl1r) .or. ( ii1r .le. 0 ) )               &
     &        call zzrc( (count*100_4) + 1_4 )

         if( ieee_is_finite(rl1i) .or. ( ii1i .le. 0 ) )               &
     &        call zzrc( (count*100_4) + 2_4 )


         ! checking cx2: (-inf, -inf)
         rl2r = real(cx2)
         rl2i = imag(cx2)

         if( ieee_is_finite(rl2r) .or. ( ii2r .ge. 0 ) )               &
     &        call zzrc( (count*100_4) + 3_4 )

         if( ieee_is_finite(rl2i) .or. ( ii2i .ge. 0 ) )               &
     &        call zzrc( (count*100_4) + 4_4 )


         ! checking cx3: (-inf, +inf)
         rl3r = real(cx3)
         rl3i = imag(cx3)

         if( ieee_is_finite(rl3r) .or. ( ii3r .ge. 0 ) )               &
     &        call zzrc( (count*100_4) + 5_4 )

         if( ieee_is_finite(rl3i) .or. ( ii3i .le. 0 ) )               &
     &        call zzrc( (count*100_4) + 6_4 )


         ! checking cx4: (+nan(s), +nan(q))
         rl4r = real(cx4)
         rl4i = imag(cx4)

         if ( ( .not. ieee_is_nan(rl4r) ) .or.                         &
     &        ( ieee_class(rl4r) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii4r .le. 0 ) ) call zzrc( (count*100_4) + 7_4 )

         if ( ( .not. ieee_is_nan(rl4i) ) .or.                         &
     &        ( ieee_class(rl4i) .ne. ieee_quiet_nan ) .or.            &
     &        ( ii4i .le. 0 ) ) call zzrc( (count*100_4) + 8_4 )

         ! checking i1 : 123
         if ( i1 .ne. 123 ) call zzrc( (count*100_4) + 9_4 )

         ! checking cx5: (+nan(q), -nan(s))
         rl5r = real(cx5)
         rl5i = imag(cx5)

         if ( ( .not. ieee_is_nan(rl5r) ) .or.                         &
     &        ( ieee_class(rl5r) .ne. ieee_quiet_nan ) .or.            &
     &        ( ii5r .le. 0 ) ) call zzrc( (count*100_4) + 10_4 )

         if ( ( .not. ieee_is_nan(rl5i) ) .or.                         &
     &        ( ieee_class(rl5i) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii5i .ge. 0 ) ) call zzrc( (count*100_4) + 11_4 )


         ! checking cx6: (-nan(s), nan(q))
         rl6r = real(cx6)
         rl6i = imag(cx6)

         if ( ( .not. ieee_is_nan(rl6r) ) .or.                         &
     &        ( ieee_class(rl6r) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii6r .ge. 0 ) ) call zzrc( (count*100_4) + 12_4 )

         if ( ( .not. ieee_is_nan(rl6i) ) .or.                         &
     &        ( ieee_class(rl6i) .ne. ieee_quiet_nan ) .or.            &
     &        ( ii6i .le. 0 ) ) call zzrc( (count*100_4) + 13_4 )


         ! checking c1 : 'iBm'
         if ( c1 .ne. 'iBm' ) call zzrc( (count*100_4) + 14_4 )

         ! checking cx7: (+inf, -nan(s))
         rl7r = real(cx7)
         rl7i = imag(cx7)

         if ( ieee_is_finite(rl7r) .or. ( ii7r .le. 0 ) )              &
     &        call zzrc( (count*100_4) + 15_4 )

         if ( ( .not. ieee_is_nan(rl7i) ) .or.                         &
     &        ( ieee_class(rl7i) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii7i .ge. 0 ) ) call zzrc( (count*100_4) + 16_4 )


         ! checking cx8: (-inf, -nan(q))
         rl8r = real(cx8)
         rl8i = imag(cx8)

         if ( ieee_is_finite(rl8r) .or. ( ii8r .ge. 0 ) )              &
     &        call zzrc( (count*100_4) + 17_4 )

         if ( ( .not. ieee_is_nan(rl8i) ) .or.                         &
     &        ( ieee_class(rl8i) .ne. ieee_quiet_nan ) .or.            &
     &        ( ii8i .ge. 0 ) ) call zzrc( (count*100_4) + 18_4 )


         ! checking cx9: (-2.0, -nan(s))
         rl9r = real(cx9)
         rl9i = imag(cx9)

         if ( .not. precision_r4(rl9r, -2.0_4) )                       &
     &        call zzrc( (count*100_4) + 19_4 )

         if ( ( .not. ieee_is_nan(rl9i) ) .or.                         &
     &        ( ieee_class(rl9i) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii9i .ge. 0 ) ) call zzrc( (count*100_4) + 20_4 )


         ! checking cx10: (-inf, +3.0)
         rl10r = real(cx10)
         rl10i = imag(cx10)

         if ( ieee_is_finite(rl10r) .or. ( ii10r .ge. 0 ) )            &
     &        call zzrc( (count*100_4) + 21_4 )

         if ( .not. precision_r4(rl10i, 3.0_4) )                       &
     &        call zzrc( (count*100_4) + 22_4 )

      end do

      if ( count .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      close(in)

      end
