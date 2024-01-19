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
!*  Testing I/O of IEEE NaN and Inf using DIRECT access mode for type
!*  COMPLEX.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: f_unit = 11

      real(kind=4) :: real_part, imag_part
      complex(4)   :: cx

      open(f_unit, file='miscNaNInfIO006.dat', access='direct',        &
     &     form='formatted', status='replace', recl=20)

      ! write 3 records in some non-sequencial order

      ! (+NaN(S),+NaN(Q))
      write(f_unit, '(sp, 2f10.4)', rec=3)  (z'7F8FFFFF', z'7FFFFFFF')

      ! (-NaN(Q),+Inf)
      write(f_unit, '(sp, 2f10.4)', rec=1)  (z'FFFFFFFF', z'7F800000')

      ! (-Inf,+NaN(S))
      write(f_unit, '(sp, 2f10.4)', rec=2)  (z'FF800000', z'7F8FFFFF')


      ! read the values from the records and verify the results

      cx  = (0.0, 0.0)
      read(f_unit, '(2f10.1)', rec=1) cx ! should be (+NaN(Q),+Inf)

      real_part = real(cx)
      imag_part = imag(cx)

      if ( ( .not. ieee_is_nan(real_part) ) .or.                       &
     &     ( ieee_class(real_part) .ne. ieee_quiet_nan ) .or.          &
     &     ( .not. equiv_is_positive(real_part) ) ) error stop 1_4

      if ( ieee_is_finite(imag_part) .or. ieee_is_negative(imag_part) )&
     &      error stop 2_4


      cx  = (0.0, 0.0)
      read(f_unit, '(2f10.1)', rec=2) cx ! should be (-Inf,+NaN(S))

      real_part = real(cx)
      imag_part = imag(cx)

      if ( ieee_is_finite(real_part) .or.                              &
     &     .not. ieee_is_negative(real_part) ) error stop 3_4

      if ( ( .not. ieee_is_nan(imag_part) ) .or.                       &
     &     ( ieee_class(imag_part) .ne. ieee_signaling_nan ) .or.      &
     &     ( .not. equiv_is_positive(imag_part) ) ) error stop 4_4


      cx  = (0.0, 0.0)
      read(f_unit, '(2f10.1)', rec=3) cx ! should be (+NaN(S),+NaN(Q))

      real_part = real(cx)
      imag_part = imag(cx)

      if ( ( .not. ieee_is_nan(real_part) ) .or.                       &
     &     ( ieee_class(real_part) .ne. ieee_signaling_nan ) .or.      &
     &     ( .not. equiv_is_positive(real_part) ) ) error stop 5_4

      if ( ( .not. ieee_is_nan(imag_part) ) .or.                       &
     &     ( ieee_class(imag_part) .ne. ieee_quiet_nan ) .or.          &
     &     ( .not. equiv_is_positive(imag_part) ) ) error stop 6_4



      ! write a few IEEE NaNs and Infs with deferent forms and check the sign

      write(f_unit, '(a20)', rec=4)  '   -NaN(Q)   -NaN(S)'
      write(f_unit, '(a20)', rec=5)  '    -NaN()      -NaN'
      write(f_unit, '(a20)', rec=6)  '   -nAn(_)+NAN(_a12)'
      write(f_unit, '(a20)', rec=7)  '  infinity -infinity'

      cx  = (0.0, 0.0)
      read(f_unit, '(2f10.5)', rec=4) cx ! should be (-NaN(Q),-NaN(S))

      real_part = real(cx)
      imag_part = imag(cx)

      if ( ( .not. ieee_is_nan(real_part) ) .or.                       &
     &     ( ieee_class(real_part) .ne. ieee_quiet_nan ) .or.          &
     &     ( .not. equiv_is_negative(real_part) ) ) error stop 7_4

      if ( ( .not. ieee_is_nan(imag_part) ) .or.                       &
     &     ( ieee_class(imag_part) .ne. ieee_signaling_nan ) .or.      &
     &     ( .not. equiv_is_negative(imag_part) ) ) error stop 8_4

      cx  = (0.0, 0.0)
      read(f_unit, '(2f10.5)', rec=5) cx ! should be (-NaN(Q),-NaN(Q))

      real_part = real(cx)
      imag_part = imag(cx)

      if ( ( .not. ieee_is_nan(real_part) ) .or.                       &
     &     ( ieee_class(real_part) .ne. ieee_quiet_nan ) .or.          &
     &     ( .not. equiv_is_negative(real_part) ) ) error stop 9_4

      if ( ( .not. ieee_is_nan(imag_part) ) .or.                       &
     &     ( ieee_class(imag_part) .ne. ieee_quiet_nan ) .or.          &
     &     ( .not. equiv_is_negative(imag_part) ) ) error stop 10_4

      cx  = (0.0, 0.0)
      read(f_unit, '(2f10.5)', rec=6) cx ! should be (-NaN(Q),+NaN(Q))

      real_part = real(cx)
      imag_part = imag(cx)

      if ( ( .not. ieee_is_nan(real_part) ) .or.                       &
     &     ( ieee_class(real_part) .ne. ieee_quiet_nan ) .or.          &
     &     ( .not. equiv_is_negative(real_part) ) ) error stop 11_4

      if ( ( .not. ieee_is_nan(imag_part) ) .or.                       &
     &     ( ieee_class(imag_part) .ne. ieee_quiet_nan ) .or.          &
     &     ( .not. equiv_is_positive(imag_part) ) ) error stop 12_4


      cx  = (0.0, 0.0)
      read(f_unit, '(2f10.5)', rec=7) cx ! should be (Inf,-Inf)

      real_part = real(cx)
      imag_part = imag(cx)

      if ( ieee_is_finite(real_part) .or. ieee_is_negative(real_part) )&
     &     error stop 13_4

      if ( ieee_is_finite(imag_part) .or.                              &
     &     .not. ieee_is_negative(imag_part) ) error stop 14_4


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
