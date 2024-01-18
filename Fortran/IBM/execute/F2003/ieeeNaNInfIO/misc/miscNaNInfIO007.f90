!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : miscNaNInfIO007.f
!*
!*  DATE                       : July 10, 2006
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
!*  Test the I/O of IEEE exceptional specifications ( NaN & Inf ) with
!*  asynchronous I/O.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: f_unit = 11

      real(kind=4) :: rl1, rl2, rl3
      real(kind=4) :: real_part, imag_part
      complex(4)   :: cx

      logical, external :: precision_r4

      open(f_unit, file='miscNaNInfIO007.dat', status='replace',       &
     &     asynchronous='yes')

      ! write +Inf, -NaN(Q), +NaN(S) respecitively
      write(f_unit, '(f10.2, D5.2, E7.3)', asynchronous='yes')         &
     &     z'7F800000', z'FFFFFFFF', z'7F8FFFFF'

      ! write +3.14, +NaN(S) and +Inf
      cx = (3.14_4, z'7F8FFFFF')
      write(f_unit, '(dc, sp, rn, 3f10.2)', asynchronous='yes')        &
     &     cx, z'7F800000'

      wait(f_unit)

      ! write +NaN(S), -Inf, (+NaN(Q), -NaN(S)), +Inf
      rl1 = z'7F8FFFFF'
      rl2 = z'FF800000'
      rl3 = z'7F800000'
      cx = (z'7FFFFFFF', z'FF8FFFFF')
      write(f_unit, *, round='nearest', asynchronous='yes')            &
     &     rl1, rl2, cx, rl3

      ! write (+Inf, -NaN(S)) +NaN(Q)
      cx = (z'7F800000', z'FF8FFFFF')
      rl1 = z'7FFFFFFF'
      write(f_unit, *, decimal='comma', asynchronous='no', sign='plus')&
     &     cx, rl1

      write(f_unit, *, asynchronous='yes')                             &
     &     ' -NaN() +infinity -nan -infinity +nan(_ab12)'


      wait(f_unit)

      rewind(f_unit)

      !***************************************************
      ! read and verify
      !***************************************************
      rl1 = 0.0; cx = (0.0, 0.0)
      read(f_unit, '(f10.2, f5.1, f7.2)', asynchronous='yes')          &
     &     rl1 , cx

      real_part = real(cx)
      imag_part = imag(cx)

      ! rl1 should be +Inf
      if ( ieee_is_finite(rl1) .or. ieee_is_negative(rl1) ) error stop 1_4

      ! real part of cx should be +NaN(Q)
      if ( ( .not. ieee_is_nan(real_part) ) .or.                       &
     &     ( ieee_class(real_part) .ne. ieee_quiet_nan ) .or.          &
     &     ( .not. equiv_is_positive(real_part) ) ) error stop 2_4

      ! imaginary part of cx should be +NaN(S)
      if ( ( .not. ieee_is_nan(imag_part) ) .or.                       &
     &     ( ieee_class(imag_part) .ne. ieee_signaling_nan ) .or.      &
     &     ( .not. equiv_is_positive(imag_part) ) ) error stop 3_4

      !****************************************************

      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0
      read(f_unit, *, decimal='comma', asynchronous='yes')             &
     &     rl1, rl2, rl3

      ! rl1 should be +3.14
      if ( .not. precision_r4(rl1, 3.14_4) ) error stop 4_4

      ! rl2 should be +NaN(S)
      if ( ( .not. ieee_is_nan(rl2) ) .or.                             &
     &     ( ieee_class(rl2) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_positive(rl2) ) ) error stop 5_4

      ! rl3 should be +Inf
      if ( ieee_is_finite(rl3) .or. ieee_is_negative(rl3) ) error stop 6_4

      !****************************************************

      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; cx = (0.0, 0.0)
      read(f_unit, *, asynchronous='yes') rl1, rl2, cx, rl3

      ! rl1 should be +nan(s)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 7_4

      ! rl2 should be -Inf
      if ( ieee_is_finite(rl2) .or. .not. ieee_is_negative(rl2) )      &
     &     error stop 8_4

      ! cx should be (+nan(q), +nan(s))
      real_part = real(cx)
      imag_part = imag(cx)

      if ( ( .not. ieee_is_nan(real_part) ) .or.                       &
     &     ( ieee_class(real_part) .ne. ieee_quiet_nan ) .or.          &
     &     ( .not. equiv_is_positive(real_part) ) ) error stop 9_4

      if ( ( .not. ieee_is_nan(imag_part) ) .or.                       &
     &     ( ieee_class(imag_part) .ne. ieee_signaling_nan ) .or.      &
     &     ( .not. equiv_is_positive(imag_part) ) ) error stop 10_4

      ! rl3 should be +inf
      if ( ieee_is_finite(rl3) .or. ieee_is_negative(rl3) )            &
     &     error stop 11_4

      wait(f_unit)

      !****************************************************

      rl1 = 0.0; cx = (0.0, 0.0)
      read(f_unit, *, asynchronous='yes', decimal='comma') cx, rl1

      ! cx should be (+Inf, +NaN(S))
      real_part = real(cx)
      imag_part = imag(cx)

      if ( ieee_is_finite(real_part) .or. ieee_is_negative(real_part)) &
     &     error stop 12_4

      if ( ( .not. ieee_is_nan(imag_part) ) .or.                       &
     &     ( ieee_class(imag_part) .ne. ieee_signaling_nan ) .or.      &
     &     ( .not. equiv_is_positive(imag_part) ) ) error stop 13_4

      ! rl1 should be +NaN(Q)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 14_4

      !****************************************************

      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; cx = (0.0, 0.0)
      read(f_unit, '(f8.2, f10.2, f5.2, f10.5, f12.1)',                &
     &     asynchronous='yes') rl1, rl2, cx, rl3

      real_part = real(cx)
      imag_part = imag(cx)

      ! rl1 should be -NaN(Q)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_negative(rl1) ) ) error stop 15_4

      ! rl2 should be +Inf
      if ( ieee_is_finite(rl2) .or. ieee_is_negative(rl2) )            &
     &     error stop 16_4

      ! cx should be (-NaN(Q),-Inf)
      if ( ( .not. ieee_is_nan(real_part) ) .or.                       &
     &     ( ieee_class(real_part) .ne. ieee_quiet_nan ) .or.          &
     &     ( .not. equiv_is_negative(real_part) ) ) error stop 17_4

      if ( ieee_is_finite( imag_part ) .or.                            &
     &     .not. ieee_is_negative( imag_part ) ) error stop 18_4

      ! rl3 should be +NaN(Q)
      if ( ( .not. ieee_is_nan(rl3) ) .or.                             &
     &     ( ieee_class(rl3) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl3) ) ) error stop 19_4


      close(f_unit)

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


      end
