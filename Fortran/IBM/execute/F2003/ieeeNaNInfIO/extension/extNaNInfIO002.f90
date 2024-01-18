!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : extNaNInfIO002.f
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
!*  by extension using SIGN mode of 'PLUS'.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program extNaNInfIO002

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11, out = 12

      complex(4) :: cx
      real(4)    :: nanq_pos, nans_pos, nanq_neg, nans_neg, inf_neg, inf_pos

      open(in,  file='extNaNInfIO002.dat', action='read')
      open(out, file='extNaNInfIO002.out', action='write')

      call setrteopts("langlvl=extended")

      ! read in and initialize the variables
      read(in, '(SP, f9.1, f6.6, f4.3)') cx, nans_pos
      read(in, '(SP, f6.2, f4.1, f10.1)') nanq_pos, inf_neg, nans_neg

      ! check the values just read

      inf_pos  = real(cx)
      nanq_neg = imag(cx)

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


      ! write out the values just read
      write(out, *, sign='plus') nanq_pos, nanq_neg, cx,  nans_pos,    &
     &                           nans_neg, inf_neg, inf_pos

      ! edit descriptor should change the default mode
      write(out, '(SP, 8F9.3)', sign='suppress') inf_pos, cx, inf_neg, &
     &     nans_neg, nans_pos, nanq_neg, nanq_pos

      write(out, '(SP, f3.0, SS, F3.0)') inf_pos, inf_pos

      write(out, '(SP, f4.1)') nanq_pos

      write(out, '(SP, f4.2)') nans_pos

      write(out, '(SP, f4.1, f2.1, f5.2)') inf_pos, nans_pos, nanq_pos

      write(out, '(SP, f3.1)') inf_neg

      write(out, '(SP, f4.0)') nanq_neg

      write(out, '(SP, f4.4)') nans_neg

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
