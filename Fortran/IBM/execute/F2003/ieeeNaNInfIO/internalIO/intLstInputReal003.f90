!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : intLstInputReal003.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 27, 2006
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
!*  Test input of IEEE NaN and Inf with internal files using list-directed I/O.
!*  In this testcase IEEE exceptional specifications are placed inside objects 
!*  of type REAL and kind 16.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      character(200) :: iFile =                                        &
     &     'nan nan(s) inf nan(q) +nAn(s) 3 -infInitY +INFinity -nan()'&
     &   //' NaN(abcd1234) nan(_) nan(__) nan(_abc123) 4 naN(abc123_) '&
     &   //'-INF +inF -nan(s)'
      
      real(16) :: rl1, rl2, rl3, rl4, rl5, rl6, rl7, rl8, rl9, rl10,   &
     &           rl11, rl12, rl13, rl14, rl15, rl16

      real(8) :: rl1e, rl2e, rl3e, rl4e, rl5e, rl6e, rl7e, rl8e, rl9e, &
     &           rl10e, rl11e, rl12e, rl13e, rl14e, rl15e, rl16e

      integer(8) :: ii1, ii2, ii3, ii4, ii5, ii6, ii7, ii8, ii9, ii10, &
     &              ii11, ii12, ii13, ii14, ii15, ii16
      
      integer :: i1, i2
      
      equivalence(rl1, ii1, rl1e); equivalence(rl2, ii2, rl2e)
      equivalence(rl3, ii3, rl3e); equivalence(rl4, ii4, rl4e)
      equivalence(rl5, ii5, rl5e); equivalence(rl6, ii6, rl6e)
      equivalence(rl7, ii7, rl7e); equivalence(rl8, ii8, rl8e)
      equivalence(rl9, ii9, rl9e); equivalence(rl10, ii10, rl10e)
      equivalence(rl11, ii11, rl11e); equivalence(rl12, ii12, rl12e)
      equivalence(rl13, ii13, rl13e); equivalence(rl14, ii14, rl14e)
      equivalence(rl15, ii15, rl15e); equivalence(rl16, ii16, rl16e)

      ! initialize all variables:
      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0; rl5 = 0.0; rl6 = 0.0
      rl7 = 0.0; rl8 = 0.0; rl9 = 0.0; rl10 = 0.0; rl11 = 0.0; rl12=0.0
      rl13 = 0.0; rl14 = 0.0; rl15 = 0.0; rl16 = 0.0


      read(iFile, *) rl1, rl2, rl3, rl4, rl5, i1, rl6, rl7, rl8, rl9,  &
     &               rl10, rl11, rl12, i2, rl13, rl14, rl15, rl16

      ! rl1 should be +NaN(Q)
      if ( ( .not. ieee_is_nan( rl1e ) ) .or.                          &
     &     ( ieee_class( rl1e ) .ne. ieee_quiet_nan ) .or.             &
     &     ( ii1 .le. 0 ) ) error stop 1_4

      ! rl2 should be +NaN(S)
      if ( ( .not. ieee_is_nan( rl2e ) ) .or.                          &
     &     ( ieee_class( rl2e ) .ne. ieee_signaling_nan ) .or.         &
     &     ( ii2 .le. 0 ) ) error stop 2_4

      ! rl3 should be +Inf
      if ( ieee_is_finite( rl3e ) .or. ieee_is_negative( rl3e ) )      &
     &     error stop 3_4
      
      ! rl4 should be +NaN(Q)
       if ( ( .not. ieee_is_nan( rl4e ) ) .or.                         &
     &     ( ieee_class( rl4e ) .ne. ieee_quiet_nan ) .or.             &
     &     ( ii4 .le. 0 ) ) error stop 4_4     

      ! rl5 should be +NaN(S)
      if ( ( .not. ieee_is_nan( rl5e ) ) .or.                          &
     &     ( ieee_class( rl5e ) .ne. ieee_signaling_nan ) .or.         &
     &     ( ii5 .le. 0 ) ) error stop 5_4

      ! i1 should be 3
      if ( i1 .ne. 3 ) error stop 6_4
      
      ! rl6 should be -Inf
      if ( ieee_is_finite( rl6e ) .or. (.not.ieee_is_negative(rl6e)) ) &
     &     error stop 7_4
      
      ! rl7 should be +Inf
      if ( ieee_is_finite( rl7e ) .or. ieee_is_negative( rl7e ) )      &
     &     error stop 8_4

      ! rl8 should be -NaN(Q)
      if ( ( .not. ieee_is_nan( rl8e ) ) .or.                          &
     &     ( ieee_class( rl8e ) .ne. ieee_quiet_nan ) .or.             &
     &     ( ii8 .ge. 0 ) ) error stop 9_4       

      ! rl9 should be +NaN(Q)
      if ( ( .not. ieee_is_nan( rl9e ) ) .or.                          &
     &     ( ieee_class( rl9e ) .ne. ieee_quiet_nan ) .or.             &
     &     ( ii9 .le. 0 ) ) error stop 10_4  

      ! rl10 should be +NaN(Q)
       if ( ( .not. ieee_is_nan( rl10e ) ) .or.                        &
     &     ( ieee_class( rl10e ) .ne. ieee_quiet_nan ) .or.            &
     &     ( ii10 .le. 0 ) ) error stop 11_4  

       ! rl11 shuld be +NaN(Q)
       if ( ( .not. ieee_is_nan( rl11e ) ) .or.                        &
     &     ( ieee_class( rl11e ) .ne. ieee_quiet_nan ) .or.            &
     &     ( ii11 .le. 0 ) ) error stop 12_4  

       ! rl12 should be +NaN(Q)
       if ( ( .not. ieee_is_nan( rl12e ) ) .or.                        &
     &     ( ieee_class( rl12e ) .ne. ieee_quiet_nan ) .or.            &
     &     ( ii12 .le. 0 ) ) error stop 13_4  

       ! i2 should be 4
       if ( i2 .ne. 4 ) error stop 14_4
       
       ! rl13 should be +NaN(Q)
       if ( ( .not. ieee_is_nan( rl13e ) ) .or.                        &
     &     ( ieee_class( rl13e ) .ne. ieee_quiet_nan ) .or.            &
     &     ( ii13 .le. 0 ) ) error stop 15_4  
       
       ! rl14 should be -Inf
       if ( ieee_is_finite(rl14e) .or. (.not.ieee_is_negative(rl14e))) &
     &      error stop 16_4
       
       ! rl15 should be +Inf
       if ( ieee_is_finite( rl15e ) .or. ieee_is_negative( rl15e ) )   &
     &      error stop 17_4

       ! rl16 should be -NaN(S)
       if ( ( .not. ieee_is_nan( rl16e ) ) .or.                        &
     &     ( ieee_class( rl16e ) .ne. ieee_signaling_nan ) .or.        &
     &     ( ii16 .ge. 0 ) ) error stop 18_4        
      
      
      end
