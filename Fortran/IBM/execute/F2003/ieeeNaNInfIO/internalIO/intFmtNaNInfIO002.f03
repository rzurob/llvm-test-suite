!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 26, 2006
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
!*  Test the sign of IEEE NaNs during format-directed input with
!*  internal files.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      real(4) :: rl1, rl2, rl3, rl4, rl5
      complex(4) :: cx

      character(50) :: iFile =                                         &
     &     '-nan +infiniTy -NaN(s) nan -inf +nan(Q) -nan()'

      read(iFile,'(f4.2, f11.0, f7.7, ES4.1, G5.3, D8.1, Q7.3)')       &
     &     rl1, rl2, rl3, rl4, rl5, cx

      if ( .not. equiv_is_negative(rl1) ) error stop 1_4
      if ( .not. equiv_is_positive(rl2) ) error stop 2_4
      if ( .not. equiv_is_negative(rl3) ) error stop 3_4
      if ( .not. equiv_is_positive(rl4) ) error stop 4_4
      if ( .not. equiv_is_negative(rl5) ) error stop 5_4
      if ( .not. equiv_is_positive(real(cx)) ) error stop 6_4
      if ( .not. equiv_is_negative(imag(cx)) ) error stop 7_4


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
