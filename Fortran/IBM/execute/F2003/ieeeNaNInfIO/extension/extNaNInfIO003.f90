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
!*  by extension using various edit descriptors'.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program extNaNInfIO003

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11, out = 12

      character(3) :: cc
      complex(4)   :: cx
      real(4)      :: nanq_pos, nans_pos, nanq_neg, nans_neg, inf_neg, inf_pos

      open(in,  file='extNaNInfIO003.dat', action='read')
      open(out, file='extNaNInfIO003.out', action='write')

      call setrteopts("langlvl=extended")

      ! read in the values using different descriptors
      read(in, '(G4.2, a2, ES4.1, rd, D4.2, EN5.1, F5.2, Q8.1, E10.5)')&
     &     nanq_pos, cc, cx, nans_neg, nans_neg, inf_pos, nanq_neg

      ! check the values just read

      inf_neg  = real(cx)
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


      ! write out the values just read
      write(out, fmt=100) nans_neg, cx, nanq_pos, inf_neg, nanq_neg,   &
     &                nans_pos, inf_pos


 100  format(ES5.5,D9.1,G5.2,ru,EN4.2,E4.1,Q5.3,F10.5,SP,G4.1)

      write(out, '(ES4.1, ES5.1, G3.2, G2.2)')                         &
     &     nans_neg, nans_neg, inf_pos, inf_pos

      write(out, '(D6.1, dc, D3.1, EN4.1, EN3.1)')                     &
     &     nans_pos, nans_pos, inf_neg, inf_neg


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
