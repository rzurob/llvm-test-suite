!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : miscNaNInfIO005.f
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
!*  Testing I/O of IEEE NaN and Inf using DIRECT access mode for type
!*  REAL.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: f_unit = 11

      real(kind=4) :: rl1

      open(f_unit, file='miscNaNInfIO005.dat', access='direct',        &
     &     form='formatted', status='replace', recl=10)

      ! write 6 records in some non-sequencial order

      write(f_unit, '(sp, f10.4)', rec=6)  z'7F8FFFFF' ! +NaN(S)

      write(f_unit, '(dc, f10.4)', rec=1)  z'FF8FFFFF' ! -NaN(S)

      write(f_unit, '(rn, f10.2)', rec=5)  z'FF800000' ! -Inf

      write(f_unit, '(sp, D10.2)', rec=2)  z'7F800000' ! +Inf

      write(f_unit, '(E10.2)', rec=4)      z'FFFFFFFF' ! -NaN(Q)

      write(f_unit, '(F10.2)', rec=3)      z'7FFFFFFF' ! +NaN(Q)

      ! read the values from the records ( starting from last record
      ! and going up ) and verify the results

      rl1 = 0.0
      read(f_unit, '(f10.1)', rec=6) rl1 ! should be +NaN(S)

      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 1_4

      rl1 = 0.0
      read(f_unit, '(f10.1)', rec=5) rl1 ! should be -Inf

      if ( ieee_is_finite(rl1) .or. .not. ieee_is_negative(rl1) ) then
         error stop 2_4
      end if

      rl1 = 0.0
      read(f_unit, '(f10.1)', rec=4) rl1 ! should be +NaN(Q)

      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 3_4

      rl1 = 0.0
      read(f_unit, '(f10.1)', rec=3) rl1 ! should be +NaN(Q)

      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 4_4

      rl1 = 0.0
      read(f_unit, '(f10.1)', rec=2) rl1 ! should be +Inf

      if ( ieee_is_finite(rl1) .or. ieee_is_negative(rl1) ) then
         error stop 5_4
      end if

      rl1 = 0.0
      read(f_unit, '(f10.1)', rec=1) rl1 ! should be +NaN(S)

      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 6_4


      ! write a few nans with deferent forms and check the sign

      write(f_unit, '(a10)', rec=7)  '-NaN(Q)'
      write(f_unit, '(a10)', rec=8)  '-NaN()'
      write(f_unit, '(a10)', rec=9)  '-NaN(s)'
      write(f_unit, '(a10)', rec=10) '-NaN(_a12)'
      write(f_unit, '(a10)', rec=11) '+NaN(_)'
      write(f_unit, '(a10)', rec=12) '+infinity'
      write(f_unit, '(a10)', rec=13) '-infinity'

      rl1 = 0.0
      read(f_unit, '(f10.5)', rec=7) rl1 ! should be -NaN(Q)

      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_negative(rl1) ) ) error stop 7_4

      rl1 = 0.0
      read(f_unit, '(f10.5)', rec=8) rl1 ! should be -NaN(Q)

      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_negative(rl1) ) ) error stop 8_4

      rl1 = 0.0
      read(f_unit, '(f10.5)', rec=9) rl1 ! should be -NaN(S)

      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_negative(rl1) ) ) error stop 9_4

      rl1 = 0.0
      read(f_unit, '(f10.5)', rec=10) rl1 ! should be -NaN(Q)

      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_negative(rl1) ) ) error stop 10_4

      rl1 = 0.0
      read(f_unit, '(f10.5)', rec=11) rl1 ! should be +NaN(Q)

      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 11_4

      rl1 = 0.0
      read(f_unit, '(f10.5)', rec=12) rl1 ! should be +Inf

      if ( ieee_is_finite(rl1) .or. ieee_is_negative(rl1) ) then
         error stop 12_4
      end if

      rl1 = 0.0
      read(f_unit, '(f10.5)', rec=13) rl1 ! should be -Inf

      if ( ieee_is_finite(rl1) .or. .not. ieee_is_negative(rl1) ) then
         error stop 13_4
      end if


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
