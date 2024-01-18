!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : extNaNInfIO001.f
!*
!*  DATE                       : July 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=oldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Testing the non-standard forms of NaN on input/output supported
!*  by extension.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program extNaNInfIO001

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11, out = 12

      complex(4) :: cx
      real(4)    :: rl1, rl2, rl3, rl4, rl5, real_part, imag_part

      character(5), parameter :: mode_r(2) = (/ "COMMA", "POINT" /)
      character(5), parameter :: mode_w(2) = (/ "POINT", "COMMA" /)

      integer :: count = 0, index = 1

      logical, external :: precision_r4

      open(in,  file='extNaNInfIO001.dat', action='read')
      open(out, file='extNaNInfIO001.out', action='write')

      call setrteopts("langlvl=extended")

      ! read in format-directed and write in list-directed
      do count=1, 4

         cx = (0.0, 0.0); rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0
         rl5 = 0.0; real_part = 0.0; imag_part = 0.0

         if ( index .eq. 1 ) then
            index = 2
         else
            index = 1
         end if

         read(in, '(7F11.2)', decimal=mode_r(index))                   &
     &        cx, rl1, rl2, rl3, rl4, rl5

         real_part = real(cx)
         imag_part = imag(cx)

         !**************************************************************
         ! check the values just read
         !**************************************************************

         ! real_part should be -NaN(S)
         if ( ( .not. ieee_is_nan( real_part ) ) .or.                 &
     &        ( ieee_class( real_part ) .ne. ieee_signaling_nan ) .or.&
     &        ( .not. equiv_is_negative( real_part ) ) ) error stop 1_4

         ! imag_part should be +NaN(Q)
         if ( ( .not. ieee_is_nan( imag_part ) ) .or.                  &
     &        ( ieee_class( imag_part ) .ne. ieee_quiet_nan ) .or.     &
     &        ( .not. equiv_is_positive( imag_part ) ) ) error stop 2_4

         ! rl1 should be +NaN(S)
         if ( ( .not. ieee_is_nan( rl1 ) ) .or.                        &
     &        ( ieee_class( rl1 ) .ne. ieee_signaling_nan ) .or.       &
     &        ( .not. equiv_is_positive( rl1 ) ) ) error stop 3_4

         ! rl2 should be 3.14
         if ( .not. precision_r4(rl2, 3.14_4) ) error stop 4_4

         ! rl3 should be -NaN(Q)
         if ( ( .not. ieee_is_nan( rl3 ) ) .or.                        &
     &        ( ieee_class( rl3 ) .ne. ieee_quiet_nan ) .or.           &
     &        ( .not. equiv_is_negative( rl3 ) ) ) error stop 5_4

         ! rl4 should be +Inf
         if ( ieee_is_finite(rl4) .or. ieee_is_negative(rl4)  )        &
     &        error stop 6_4

         ! rl5 should be -Inf
         if ( ieee_is_finite(rl5) .or. .not. ieee_is_negative(rl5) )   &
     &        error stop 7_4

         ! write out the values in the old format for verification.
         write(out, *, decimal=mode_w(index))                          &
     &        cx, rl1, rl2, rl3, rl4, rl5

      end do

      ! read in list-directed and write in format-directed
      do count=1, 4

         cx = (0.0, 0.0); rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0
         rl5 = 0.0; real_part = 0.0; imag_part = 0.0

         if ( index .eq. 1 ) then
            index = 2
         else
            index = 1
         end if

         read(in, *, decimal=mode_r(index))                            &
     &        cx, rl1, rl2, rl3, rl4, rl5

         real_part = real(cx)
         imag_part = imag(cx)


         !**************************************************************
         ! check the values just read
         !**************************************************************
         ! real_part should be -NaN(S)
         if ( ( .not. ieee_is_nan( real_part ) ) .or.                 &
     &        ( ieee_class( real_part ) .ne. ieee_signaling_nan ) .or.&
     &        ( .not. equiv_is_negative( real_part ) ) ) error stop 8_4

         ! imag_part should be +NaN(Q)
         if ( ( .not. ieee_is_nan( imag_part ) ) .or.                  &
     &        ( ieee_class( imag_part ) .ne. ieee_quiet_nan ) .or.     &
     &        ( .not. equiv_is_positive( imag_part ) ) ) error stop 9_4

         ! rl1 should be +NaN(S)
         if ( ( .not. ieee_is_nan( rl1 ) ) .or.                        &
     &        ( ieee_class( rl1 ) .ne. ieee_signaling_nan ) .or.       &
     &        ( .not. equiv_is_positive( rl1 ) ) ) error stop 10_4

         ! rl2 should be 3.14
         if ( .not. precision_r4(rl2, 3.14_4) ) error stop 11_4

         ! rl3 should be -NaN(Q)
         if ( ( .not. ieee_is_nan( rl3 ) ) .or.                        &
     &        ( ieee_class( rl3 ) .ne. ieee_quiet_nan ) .or.           &
     &        ( .not. equiv_is_negative( rl3 ) ) ) error stop 12_4

         ! rl4 should be +Inf
         if ( ieee_is_finite(rl4) .or. ieee_is_negative(rl4)  )        &
     &        error stop 13_4

         ! rl5 should be -Inf
         if ( ieee_is_finite(rl5) .or. .not. ieee_is_negative(rl5) )   &
     &        error stop 14_4


         ! write the values out in old format for verification
         write(out, '(7F12.2)', decimal=mode_w(index))                 &
     &        cx, rl1, rl2, rl3, rl4, rl5

      end do

      close(in)
      close(out)

      contains

      ! Returns true if the integer equivalence of
      ! the copy of the dummy argument is negative
      logical function equiv_is_negative(val)

         real(4)    :: val, tmp_val
         integer(4) :: val_eq

         equivalence(tmp_val, val_eq)

         tmp_val = val

         if ( val_eq .ge. 0 ) then
            equiv_is_negative = .false.
         else
            equiv_is_negative = .true.
         end if

      end function

      ! Returns true if the integer equivalence of
      ! the copy of the dummy argument is positive
      logical function equiv_is_positive(val)

         real(4)    :: val, tmp_val
         integer(4) :: val_eq

         equivalence(tmp_val, val_eq)

         tmp_val = val

         if ( val_eq .le. 0 ) then
            equiv_is_positive = .false.
         else
            equiv_is_positive = .true.
         end if

      end function

end program
