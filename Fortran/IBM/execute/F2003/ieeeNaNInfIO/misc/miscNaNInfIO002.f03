!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 5, 2006
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
!*  Perform cross-feature testing for IEEE NaN & Inf I/O with
!*   1) run-time encoding of format specification
!*   2) allocatable real values
!*   3) using DOUBLE PRECISION keyword
!*   4) file status of SCRATCH.
!*   5) using REWIND and BACKSPACE stmts.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter            :: f_unit = 12
      integer, parameter            :: dp_kind = kind(1d0)

      character(50)                 :: my_fmt

      double complex, allocatable   :: cx
      double precision, allocatable :: rl1, rl2, rl3, rl4

      double precision              :: real_part, imag_part

      double precision              :: real_tmp1, real_tmp2, real_tmp3
      integer(dp_kind)              :: int_tmp1, int_tmp2, int_tmp3

      equivalence(real_tmp1, int_tmp1)
      equivalence(real_tmp2, int_tmp2)
      equivalence(real_tmp3, int_tmp3)

      open(f_unit, status='scratch')

      allocate(rl1, rl2, rl3, rl4, cx)

      if ( dp_kind .eq. 8 ) then
         rl1 = z'7FF0000000000000' ! +Inf
         rl2 = z'FFF0FFFFFFFFFFFF' ! -NaN(S)
         rl3 = z'FFF0000000000000' ! -Inf
         rl4 = z'7FFFFFFFFFFFFFFF' ! +NaN(Q)
      else if ( dp_kind .eq. 4 ) then
         rl1 = b'01111111100000000000000000000000' ! +Inf
         rl2 = b'11111111101111111111111111111111' ! -NaN(S)
         rl3 = b'11111111100000000000000000000000' ! -Inf
         rl4 = b'01111111111111111111111111111111' ! +NaN(Q)
      else
         error stop 255_4 ! kind of double precision
                        ! not supported for this testcase
      end if

      cx = ( rl2, rl3 )

      my_fmt = '(SP, F12.5, E10.2E5, G9.8, ES11.4, 2F10.4)'

      write(f_unit, fmt=my_fmt) rl1, rl2, rl3, rl4, cx

      deallocate(rl1, rl2, rl3, rl4, cx)

      backspace f_unit

      allocate(rl1, rl2, rl3, rl4, cx)

      ! reset variables
      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0; cx = (0.0, 0.0)

      read(f_unit, fmt=my_fmt) rl1, rl2, rl3, rl4, cx


      ! validate the values read back in.

      ! rl1 should be +Inf
      if ( ieee_is_finite(rl1) .or. ieee_is_negative(rl1) ) then
         error stop 1_4
      end if

      ! rl2 should be NaN(S)
      if ( ( .not. ieee_is_nan(rl2) ) .or.                             &
     &     ( ieee_class(rl2) .ne. ieee_signaling_nan ) ) error stop 2_4

      ! rl3 should be -Inf
      if ( ieee_is_finite(rl3) .or. .not. ieee_is_negative(rl3) ) then
         error stop 3_4
      end if

      ! rl4 should be NaN(Q)
      if ( ( .not. ieee_is_nan(rl4) ) .or.                             &
     &     ( ieee_class(rl4) .ne. ieee_quiet_nan ) ) error stop 4_4

      ! cx should be ( NaN(S), -Inf )
      real_part = real(cx)
      imag_part = imag(cx)

      if ( ( .not. ieee_is_nan(real_part) ) .or.                       &
     &     ( ieee_class(real_part) .ne. ieee_signaling_nan ) )         &
     &     error stop 5_4

      if ( ieee_is_finite(imag_part) .or.                              &
     &     ( .not. ieee_is_negative(imag_part) ) ) error stop 6_4


      deallocate(rl1, rl2, rl3, rl4, cx)

      !********************************************************
      ! rewind the file and repeat the process
      !********************************************************

      rewind f_unit

      allocate(rl1, rl2, rl3, rl4, cx)

      ! reset variables
      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0; cx = (0.0, 0.0)

      my_fmt = '(DC, E12.5, F10.2, EN9.8, F11.4, 2F10.4)'

      read(f_unit, fmt=my_fmt) rl1, rl2, rl3, rl4, cx


      ! validate the values read back in.

      ! rl1 should be +Inf
      if ( ieee_is_finite(rl1) .or. ieee_is_negative(rl1) ) then
         error stop 7_4
      end if

      ! rl2 should be NaN(S)
      if ( ( .not. ieee_is_nan(rl2) ) .or.                             &
     &     ( ieee_class(rl2) .ne. ieee_signaling_nan ) ) error stop 8_4

      ! rl3 should be -Inf
      if ( ieee_is_finite(rl3) .or. .not. ieee_is_negative(rl3) ) then
         error stop 9_4
      end if

      ! rl4 should be NaN(Q)
      if ( ( .not. ieee_is_nan(rl4) ) .or.                             &
     &     ( ieee_class(rl4) .ne. ieee_quiet_nan ) ) error stop 10_4

      ! cx should be ( NaN(S), -Inf )
      real_part = dreal(cx)
      imag_part = dimag(cx)

      if ( ( .not. ieee_is_nan(real_part) ) .or.                       &
     &     ( ieee_class(real_part) .ne. ieee_signaling_nan ) )         &
     &     error stop 11_4

      if ( ieee_is_finite(imag_part) .or.                              &
     &     ( .not. ieee_is_negative(imag_part) ) ) error stop 12_4


      !******************************************************
      !* check that the sign of NaN and Inf are preserved
      !* on input with this file...
      !******************************************************
      deallocate(rl1, rl2, rl3)
      allocate  (rl1, rl2, rl3)
      rl1 = 0.0
      rl2 = 0.0
      rl3 = 0.0

      write(f_unit, *, delim='none') '-nan(s) -nan()  nan'

      my_fmt = '(3F8.2)'

      backspace f_unit

      read(f_unit, fmt=my_fmt) rl1, rl2, rl3

      ! rl1 should be -NaN(S)
      real_tmp1 = rl1
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_signaling_nan ) .or.            &
     &     ( int_tmp1 .ge. 0 ) ) error stop 13_4

      ! rl2 should be -NaN(Q)
      real_tmp2 = rl2
      if ( ( .not. ieee_is_nan(rl2) ) .or.                             &
     &     ( ieee_class(rl2) .ne. ieee_quiet_nan ) .or.                &
     &     ( int_tmp2 .ge. 0 ) ) error stop 14_4

      ! rl3 should be +NaN(Q)
      real_tmp3 = rl3
      if ( ( .not. ieee_is_nan(rl3) ) .or.                             &
     &     ( ieee_class(rl3) .ne. ieee_quiet_nan ) .or.                &
     &     ( int_tmp3 .le. 0 ) ) error stop 15_4


      deallocate(rl1, rl2, rl3, rl4, cx)

      close(f_unit)

      end
