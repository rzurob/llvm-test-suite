!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 6, 2006
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
!*  Testing I/O of IEEE NaN and Inf using STREAM access mode and
!*  using advancing and non-advancing I/O.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: f_unit = 11

      real(kind=4) :: rl1 = 0.0
      integer      :: i = 1

      open(f_unit, file='miscNaNInfIO004.dat', access='stream',        &
     &     form='formatted', status='replace')

      write(f_unit, '(sp, f7.2)', advance='no', pos=i)                 &
     &     b'01111111101111111111111111111111' ! +NaN(S)

      read(f_unit, '(f7.2)', advance='no', pos=i) rl1

      ! rl1 should be +NaN(S)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 1_4

      write(f_unit, '(ES6.2)', advance='no', pos=i+3)                  &
     &     b'11111111111111111111111111111111' ! -NaN(Q)

      rl1 = 0.0
      read(f_unit, '(G4.0)', advance='no', pos=i) rl1

      ! rl1 should be +NaN(Q)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 2_4

      inquire(f_unit, pos=i)

      rl1 = 0.0
      read(f_unit, '(D6.2)', advance='yes', pos=i-1) rl1

      ! rl1 should be +NaN(Q)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 3_4

      inquire(f_unit, pos=i)

      write(f_unit, '(f3.2)',advance='yes', pos=i)                     &
     &     b'11111111100000000000000000000000' ! -Inf

      inquire(f_unit, pos=i)

      write(f_unit, '(f4.2)',advance='yes', pos=i)                     &
     &     b'11111111100000000000000000000000' ! -Inf

      inquire(f_unit, pos=i)

      rl1 = 0.0
      read(f_unit, '(f4.2)', pos=i-5) rl1

      ! rl1 should be -Inf
      if ( ieee_is_finite(rl1) .or. .not. ieee_is_negative(rl1) ) then
         error stop 4_4
      end if

      rl1 = 0.0
      read(f_unit, *, pos=i-5) rl1

      ! rl1 should be -Inf
      if ( ieee_is_finite(rl1) .or. .not. ieee_is_negative(rl1) ) then
         error stop 5_4
      end if


      write(f_unit, '(a14)', advance='yes', pos=i) '-nan(s) -nan()'

      rl1 = 0.0
      read(f_unit, '(f7.2)', advance='no', pos=i) rl1

      ! rl1 should be -NaN(S)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_negative(rl1) ) ) error stop 6_4

      inquire(f_unit, pos=i)

      rl1 = 0.0
      read(f_unit, '(f7.2)', advance='yes', pos=i) rl1

      ! rl1 should be -NaN(Q)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_negative(rl1) ) ) error stop 7_4

      inquire(f_unit, pos=i)

      write(f_unit, *, pos=i) rl1

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
