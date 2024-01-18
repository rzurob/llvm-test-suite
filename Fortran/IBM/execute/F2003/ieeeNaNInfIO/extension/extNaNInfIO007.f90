!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : extNaNInfIO007.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : July 18, 2006
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
!*  Testing a mixture of input data items with different types and IEEE
!*  NaN and Inf input with various formats including the extensions. This
!*  testcase specially tests items with no value separators on the input.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program extNaNInfIO007

      use, intrinsic :: ieee_arithmetic
      implicit none
      
      integer, parameter :: in = 11

      real(kind=4) :: rl1, rl2, rl3, rl4, rl5, rl6, rl7, rl8, rl9
      integer      :: ii1, ii2
      character(5) :: cc1, cc2, cc3, cc4, cc5

      logical, external :: precision_r4
      
      open(in,  file='extNaNInfIO007.dat')

      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; ii1 = 0; ii2 = 0;
      cc1 = 'xxxxx'; cc2 = 'xxxxx'; cc3 = 'xxxxx'; cc4 = 'xxxxx'
      cc5 = 'xxxxx'

      read(in, 111) rl1, cc1, rl2, cc2, rl3, rl4, rl5, ii1, rl6, cc3,  &
     &              rl7, cc4, rl8, rl9, cc5, ii2
      
 111  format(f3.2, a3, g3.1, G5, EN4.1, f7.7,D3.1, i1, E4.2, a2,       &
     &       ES3.3, a2, f5.1, G6.2, g2, i4)
      
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 1_4

      if ( cc1 .ne. 'abc  ' ) error stop 2_4
      
      if ( ieee_is_finite(rl2) .or. ieee_is_negative(rl2) )            &
     &     error stop 3_4
      
      if ( cc2 .ne. 'inity' ) error stop 4_4
      
      if ( ( .not. ieee_is_nan(rl3) ) .or.                             &
     &     ( ieee_class(rl3) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl3) ) ) error stop 5_4
      
      if ( ( .not. ieee_is_nan(rl4) ) .or.                             &
     &     ( ieee_class(rl4) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_negative(rl4) ) ) error stop 6_4
      
      if ( ( .not. ieee_is_nan(rl5) ) .or.                             &
     &     ( ieee_class(rl5) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl5) ) ) error stop 7_4

      if ( ii1 .ne. 2 ) error stop 8_4
      
      if ( ieee_is_finite(rl6) .or. .not. ieee_is_negative(rl6) )      &
     &     error stop 9_4
      
      if ( cc3 .ne. '()   ' ) error stop 10_4
      
      if ( ( .not. ieee_is_nan(rl7) ) .or.                             &
     &     ( ieee_class(rl7) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl7) ) ) error stop 11_4

      if ( cc4 .ne. '()   ' ) error stop 12_4
      
      if ( ( .not. ieee_is_nan(rl8) ) .or.                             &
     &     ( ieee_class(rl8) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_positive(rl8) ) ) error stop 13_4

      if ( ( .not. ieee_is_nan(rl9) ) .or.                             &
     &     ( ieee_class(rl9) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl9) ) ) error stop 14_4
      

      if ( cc5 .ne. '()   ' ) error stop 15_4
      
      if ( ii2 .ne. -787 ) error stop 16_4


      ! READ THE SECOND LINE
      
      read(in, '(f5.2, g1, g9.2, g4.1, g4.2)') rl1, cc1, rl2, rl3, rl4
      
      if ( ( .not. ieee_is_nan(rl1) ) .or.                             &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.                &
     &     ( .not. equiv_is_positive(rl1) ) ) error stop 17_4
      
      if ( cc1 .ne. 'S    ' ) error stop 18_4
      
      if ( ieee_is_finite(rl2) .or. .not. ieee_is_negative(rl2) )      &
     &     error stop 19_4

      if ( ( .not. ieee_is_nan(rl3) ) .or.                             &
     &     ( ieee_class(rl3) .ne. ieee_signaling_nan ) .or.            &
     &     ( .not. equiv_is_positive(rl3) ) ) error stop 20_4
      
      if ( .not. precision_r4(rl4, 3.14_4) ) error stop 21_4
      
      close(in)
      
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
