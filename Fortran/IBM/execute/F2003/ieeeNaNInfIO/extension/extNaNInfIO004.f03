!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 17, 2006
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
!*  by extension using namelist I/O.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program extNaNInfIO004

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11, out = 12

      character(3) :: cc
      complex(4)   :: cx
      real(4)      :: nanq_pos, nans_pos, nanq_neg, nans_neg, inf_neg, inf_pos
      real(4)      :: my_pi
      integer      :: i1

      logical, external :: precision_r4

      namelist /nan/ cc, nanq_pos, i1, inf_neg, cx,                   &
     &               my_pi, nans_neg, inf_pos

      open(in,  file='extNaNInfIO004.dat', action='read')
      open(out, file='extNaNInfIO004.out', action='write')

      call setrteopts("langlvl=extended")

      ! read in the values and initialize items inside the namelist
      read(in, nan)


      ! check the values just read

      nanq_neg  = real(cx)
      nans_pos = imag(cx)

      if( ieee_is_finite(inf_pos) .or. ieee_is_negative(inf_pos) )     &
     &     error stop 1_4

      if( ( .not. ieee_is_nan(nanq_neg) ) .or.                         &
     &    ( ieee_class(nanq_neg) .ne. ieee_quiet_nan ) .or.            &
     &    ( .not. equiv_is_negative(nanq_neg) ) ) error stop 2_4

      if( ( .not. ieee_is_nan(nans_pos) ) .or.                         &
     &    (ieee_class(nans_pos) .ne. ieee_signaling_nan ) .or.         &
     &    ( .not. equiv_is_positive(nans_pos) ) ) error stop 3_4

      if( ( .not. ieee_is_nan(nanq_pos) ) .or.                         &
     &    ( ieee_class(nanq_pos) .ne. ieee_quiet_nan ) .or.            &
     &    ( .not. equiv_is_positive(nanq_pos) ) ) error stop 4_4

      if(ieee_is_finite(inf_neg) .or. .not. ieee_is_negative(inf_neg)) &
     &     error stop 5_4

      if( ( .not. ieee_is_nan(nans_neg) ) .or.                         &
     &    (ieee_class(nans_neg) .ne. ieee_signaling_nan) .or.          &
     &    ( .not. equiv_is_negative(nans_neg) ) ) error stop 6_4

      if ( cc .ne. 'xLf' ) error stop 7_4

      if ( i1 .ne. 2003 ) error stop 8_4

      if ( .not. precision_r4(my_pi, 3.14_4) ) error stop 9_4


      ! write out the namelist
      write(out, nan)
      write(out, nan, sign='plus', decimal='comma', delim='quote')

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
