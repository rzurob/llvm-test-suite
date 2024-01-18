!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : lstInputReal003.f
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
!*  are placed inside objects of type REAL and kind 16.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11

      real(16)      :: rl1, rl2, rl3, rl4, rl5, rl6
      real(8)      :: rl1e, rl2e, rl3e, rl4e, rl5e, rl6e
      integer(8)   :: ii1, ii2, ii3, ii4, ii5, ii6
      integer      :: i1, i2
      character(3) :: c1, c2

      integer(4) :: ios = 0, count = 0

      equivalence(rl1, ii1, rl1e)
      equivalence(rl2, ii2, rl2e)
      equivalence(rl3, ii3, rl3e)
      equivalence(rl4, ii4, rl4e)
      equivalence(rl5, ii5, rl5e)
      equivalence(rl6, ii6, rl6e)

      open(in, file='lstInputReal003.dat', action='read')

      do
         count = count + 1

         ! reset variables
         rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0
         rl5 = 0.0; rl6 = 0.0
         i1 = 0; i2 = 0
         c1 = 'xxx'; c2 = 'xxx'

         ! read in the values
         read(in,*, iostat=ios)                                       &
     &        rl1, i1, rl2, c1, rl3, i2, rl4, c2, rl5, rl6

         if ( is_iostat_end(ios) ) exit ! end of file - leave the loop

         ! validate the values read into the variables one by one
         ! The 2 least significant digits of the return value indicate
         ! which item in the input list caused the error.
         ! The most significant digits represent the input count number.

         ! rl1 must be +Inf
         if ( ieee_is_finite(rl1e) .or. ( ii1 .le. 0 ) )               &
     &        call zzrc( (count*100_4) + 1_4 )

         ! i1 must be 123
         if ( i1 .ne. 123 ) call zzrc((count*100_4) + 2_4 )

         ! rl2 must be -Inf
         if ( ieee_is_finite(rl2e) .or. ( ii2 .ge. 0 ) )               &
     &        call zzrc( (count*100_4) + 3_4 )

         ! c1 must be 'iBm'
         if ( c1 .ne. 'iBm' ) call zzrc((count*100_4) + 4_4 )

         ! rl3 must be +NaN(Q)
         if ( ( .not. ieee_is_nan(rl3e) ) .or.                         &
     &        ( ieee_class(rl3e) .ne. ieee_quiet_nan ) .or.            &
     &        ( ii3 .le. 0 ) ) call zzrc( (count*100_4) + 5_4 )

         ! i2 must be 456
         if ( i2 .ne. 456 ) call zzrc((count*100_4) + 6_4 )

         ! rl4 must be -NaN(Q)
         if ( ( .not. ieee_is_nan(rl4e) ) .or.                         &
     &        ( ieee_class(rl4e) .ne. ieee_quiet_nan ) .or.            &
     &        ( ii4 .ge. 0 ) ) call zzrc( (count*100_4) + 7_4 )

         ! c2 must be 'XlF'
         if ( c2 .ne. 'XlF' ) call zzrc((count*100_4) + 8_4 )

         ! rl5 must be +NaN(S)
         if ( ( .not. ieee_is_nan(rl5e) ) .or.                         &
     &        ( ieee_class(rl5e) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii5 .le. 0 ) ) call zzrc( (count*100_4) + 9_4 )

         ! rl6 must be -NaN(S)
         if ( ( .not. ieee_is_nan(rl6e) ) .or.                         &
     &        ( ieee_class(rl6e) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii6 .ge. 0 ) ) call zzrc( (count*100_4) + 10_4 )


      end do

      if ( count .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      close(in)

      end
