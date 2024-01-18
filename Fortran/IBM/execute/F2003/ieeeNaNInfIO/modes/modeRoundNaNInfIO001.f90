!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : modeRoundNaNInfIO001.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : July 4, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Handling IEEE Infinity and NAN in real/complex editing
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 311684
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nooldnaninf
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Testing the input and output of NaN and Inf with various ROUND modes
!*  for format-directed I/O
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11, out = 12
      
      character(24), parameter :: fname_in = 'modeRoundNaNInfIO001.dat'
      character(24), parameter :: fname_out = 'modeRoundNaNInfIO001.out'

      real(4)    :: rl1, rl2, rl3, rl4
      
      open(in,  file=fname_in, action='read', round='nearest')
      open(out, file=fname_out, action='write', round='zero')

      !*******************************************************
      !** READ THE FIRST LINE OF INPUT AND VERIFY
      !*******************************************************

      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0
      read(in, '(f7.2, f8.1, 2f5.2 )', round='up') rl1, rl2, rl3, rl4
      
      ! rl1 should be +NaN(Q)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 1_4
      
      ! rl2 should be -NaN(S)
      if ( ( .not. ieee_is_nan(rl2) ) .or.                             &
     &     ( ieee_class(rl2) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_negative(rl2) ) ) error stop 2_4

      ! check rl3
      if ( rl3 .ne. b'01000000010010001111010111000011' ) error stop 3_4

      ! rl4 should be +Inf
      if ( ieee_is_finite(rl4) .or. ieee_is_negative(rl4) ) error stop 4_4

      write(out, '(dc, f5.2, rn, f7.1, rd, f7.2, ru, f7.2, rz, f9.0 )',&
     &     sign='plus') rl1, rl2, rl3, rl3, rl4
      
      write(out, '(rp, f4.1, f2.1, sp, f3.1, ss, f3.1)')               &
     &     rl1, rl1, rl4, rl4
      

      !*******************************************************
      !** READ THE SECOND LINE OF INPUT AND VERIFY
      !*******************************************************

      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0
      read(in, '(f4.1, f5.2, bz, 2f8.1)', round='down') rl1, rl2, rl3, rl4

      ! rl1 should be -Inf
      if ( ieee_is_finite(rl1) .or. .not. ieee_is_negative(rl1) )      &
     &     error stop 5_4
      
      ! check rl2
      if ( rl2 .ne. b'01000000010010001111010111000010' ) error stop 6_4

      ! rl3 should be +NaN(S)
      if ( ( .not. ieee_is_nan(rl3) ) .or.                             &
     &     ( ieee_class(rl3) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_positive(rl3) ) ) error stop 7_4

      ! rl4 should be -NaN(Q)
      if ( ( .not. ieee_is_nan(rl4) ) .or.                             &
     &     ( ieee_class(rl4) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_negative(rl4) ) ) error stop 8_4
      
      write(out, '(rz, f5.2, ru, f4.1, rd, f7.2, rn, f7.2 )')          &
     &     rl1, rl2, rl3, rl4
      
      !*******************************************************
      !** READ THE THIRD LINE OF INPUT AND VERIFY
      !*******************************************************

      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0
      read(in, '(rz, f15.2, rn, f7.7, ru, 2f5.2)') rl1, rl2, rl3, rl4

      ! rl1 should be +NaN(Q)
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 9_4
      
      ! rl2 should be -NaN(Q)
      if ( ( .not. ieee_is_nan(rl2) ) .or.                             &
     &     ( ieee_class(rl2) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_negative(rl2) ) ) error stop 10_4

      ! check rl3
      if ( rl3 .ne. b'01000000010010001111010111000011' ) error stop 11_4
      
      ! rl4 should be +NaN(Q)
      if ( ( .not. ieee_is_nan(rl4) ) .or.                             &
     &     ( ieee_class(rl4) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl4) ) ) error stop 12_4

      write(out, '(4f10.1)', round='up') rl1, rl2, rl3, rl4
      
      !*******************************************************
      !** READ THE FOURTH LINE OF INPUT AND VERIFY
      !*******************************************************

      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0
      read(in, '(rc, f10.7, rp, f5.2, rd, dc, f5.2, ru, f12.4)')       &
     &     rl1, rl2, rl3, rl4
    
      ! rl1 should be +Inf
      if ( ieee_is_finite(rl1) .or. ieee_is_negative(rl1) ) error stop 13_4
      
      ! rl2 should be -NaN(Q)
      if ( ( .not. ieee_is_nan(rl2) ) .or.                             &
     &     ( ieee_class(rl2) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_negative(rl2) ) ) error stop 14_4
      
      ! check rl3
      if ( rl3 .ne. b'01000000010010001111010111000010' ) error stop 15_4

      ! rl4 should be -Inf
      if ( ieee_is_finite(rl4) .or. .not. ieee_is_negative(rl4) )      &
     &     error stop 16_4

      write(out, '(4f10.1)', round='down') rl1, rl2, rl3, rl4

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

      
      end
