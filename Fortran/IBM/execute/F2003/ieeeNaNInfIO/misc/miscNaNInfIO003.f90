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
!*  Testing IBM extension for value separator when doing input with
!*  NaN and Inf with various DECIMAL edit modes.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11

      real(kind=4) :: nanq_pos, nanq_neg, nans_pos, nans_neg,          &
     &                inf_pos, inf_neg

      open(in, file='miscNaNInfIO003.dat', decimal='point')

      ! reset variables
      nanq_pos = 0.0; nanq_neg = 0.0; nans_pos = 0.0; nans_neg = 0.0
      inf_pos  = 0.0; inf_neg  = 0.0

      read(in, '(dp, 6f30.6)') nanq_pos, nanq_neg, nans_pos, nans_neg, &
     &                         inf_pos, inf_neg

      if ( .not. is_nanq_pos( nanq_pos ) ) error stop 1_4
      if ( .not. is_nanq_neg( nanq_neg ) ) error stop 2_4
      if ( .not. is_nans_pos( nans_pos ) ) error stop 3_4
      if ( .not. is_nans_neg( nans_neg ) ) error stop 4_4
      if ( .not. is_inf_pos( inf_pos ) )   error stop 5_4
      if ( .not. is_inf_neg( inf_neg ) )   error stop 6_4

      ! reset variables
      nanq_pos = 0.0; nanq_neg = 0.0; nans_pos = 0.0; nans_neg = 0.0
      inf_pos  = 0.0; inf_neg  = 0.0

      read(in, '(dc, 6f30.6)', decimal='comma')                        &
     &     nanq_pos, nanq_neg, nans_pos, nans_neg, inf_pos, inf_neg

      if ( .not. is_nanq_pos( nanq_pos ) ) error stop 7_4
      if ( .not. is_nanq_neg( nanq_neg ) ) error stop 8_4
      if ( .not. is_nans_pos( nans_pos ) ) error stop 9_4
      if ( .not. is_nans_neg( nans_neg ) ) error stop 10_4
      if ( .not. is_inf_pos( inf_pos ) )   error stop 11_4
      if ( .not. is_inf_neg( inf_neg ) )   error stop 12_4

      ! reset variables
      nanq_pos = 0.0; nanq_neg = 0.0; nans_pos = 0.0; nans_neg = 0.0
      inf_pos  = 0.0; inf_neg  = 0.0

      read(in, '(dc, bz, sp, 6f40.0)')                                 &
     &     nanq_pos, nanq_neg, nans_pos, nans_neg, inf_pos, inf_neg

      if ( .not. is_nanq_pos( nanq_pos ) ) error stop 13_4
      if ( .not. is_nanq_neg( nanq_neg ) ) error stop 14_4
      if ( .not. is_nans_pos( nans_pos ) ) error stop 15_4
      if ( .not. is_nans_neg( nans_neg ) ) error stop 16_4
      if ( .not. is_inf_pos( inf_pos ) )   error stop 17_4
      if ( .not. is_inf_neg( inf_neg ) )   error stop 18_4


      contains

      ! Returns true if val == +NaN(Q)
      logical function is_nanq_pos(val)
         real(kind=4)             :: val
         real(kind=4)             :: real_tmp
         integer(kind=4)          :: int_tmp

         equivalence(real_tmp, int_tmp)

         real_tmp = val

         if ( ( .not. ieee_is_nan(val) ) .or.                          &
     &        ( ieee_class(val) .ne. ieee_quiet_nan ) .or.             &
     &        ( int_tmp .le. 0 ) ) then
            is_nanq_pos = .false.
         else
            is_nanq_pos = .true.
         end if
      end function

      ! Returns true if val == -NaN(Q)
      logical function is_nanq_neg(val)
         real(kind=4)             :: val
         real(kind=4)             :: real_tmp
         integer(kind=4)          :: int_tmp

         equivalence(real_tmp, int_tmp)

         real_tmp = val

         if ( ( .not. ieee_is_nan(val) ) .or.                          &
     &        ( ieee_class(val) .ne. ieee_quiet_nan ) .or.             &
     &        ( int_tmp .ge. 0 ) ) then
            is_nanq_neg = .false.
         else
            is_nanq_neg = .true.
         end if
      end function

      ! Returns true if val == +NaN(S)
      logical function is_nans_pos(val)
         real(kind=4)             :: val
         real(kind=4)             :: real_tmp
         integer(kind=4)          :: int_tmp

         equivalence(real_tmp, int_tmp)

         real_tmp = val

         if ( ( .not. ieee_is_nan(val) ) .or.                          &
     &        ( ieee_class(val) .ne. ieee_signaling_nan ) .or.         &
     &        ( int_tmp .le. 0 ) ) then
            is_nans_pos = .false.
         else
            is_nans_pos = .true.
         end if
      end function

      ! Returns true if val == -NaN(S)
      logical function is_nans_neg(val)
         real(kind=4)             :: val
         real(kind=4)             :: real_tmp
         integer(kind=4)          :: int_tmp

         equivalence(real_tmp, int_tmp)

         real_tmp = val

         if ( ( .not. ieee_is_nan(val) ) .or.                          &
     &        ( ieee_class(val) .ne. ieee_signaling_nan ) .or.         &
     &        ( int_tmp .ge. 0 ) ) then
            is_nans_neg = .false.
         else
            is_nans_neg = .true.
         end if
      end function

      ! Returns true if val == +Inf
      logical function is_inf_pos(val)
         real(kind=4), intent(in) :: val
         if ( ieee_is_finite(val) .or. ieee_is_negative(val) ) then
            is_inf_pos = .false.
         else
            is_inf_pos = .true.
         end if
      end function

      ! Returns true if val == -Inf
      logical function is_inf_neg(val)
         real(kind=4), intent(in) :: val
         if( ieee_is_finite(val) .or. (.not. ieee_is_negative(val)) )  &
     &   then
            is_inf_neg = .false.
         else
            is_inf_neg = .true.
         end if
      end function

      end
