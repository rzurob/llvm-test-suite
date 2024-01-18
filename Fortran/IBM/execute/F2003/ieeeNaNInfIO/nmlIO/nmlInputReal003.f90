!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nmlInputReal003.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 21, 2006
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
!*  Test input of IEEE NaN and Inf for namelist I/O.
!*  In this testcase IEEE exceptional specifications
!*  are placed inside objects of type real and kind 16.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic
      implicit none

      integer, parameter :: in = 11

      integer(4) :: inputno ! keeps track of the namelist input in the input file
      integer :: ios

      real(16)      :: rl1, rl2, rl3, rl4, rl5, rl6, rl7
      real(8)       :: rl1e, rl2e, rl3e, rl4e, rl5e, rl6e, rl7e
      integer(8)   :: ii1, ii2, ii3, ii4, ii5, ii6, ii7
      integer      :: i1, i2
      character(3) :: c1, c2
      
      logical precision_r6
      
      equivalence(ii1, rl1, rl1e)
      equivalence(ii2, rl2, rl2e)
      equivalence(ii3, rl3, rl3e)
      equivalence(ii4, rl4, rl4e)
      equivalence(ii5, rl5, rl5e)
      equivalence(ii6, rl6, rl6e)
      equivalence(ii7, rl7, rl7e)
      
      namelist /mynml/ i1, rl1, rl2, c1, rl3, i2, rl4, c2, rl5, rl6, rl7
      
      open(in, file='nmlInputReal003.dat', action='read')

      inputno = 0
      
      do
         inputno = inputno + 1

         rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0; rl5 = 0.0
         rl6 = 0.0; rl7 = 0.0; i1 = 0; i2 = 0; c1 = 'xxx'; c2 = 'xxx'
        
         read(in,nml=mynml,iostat=ios)
         
         if ( is_iostat_end(ios) ) exit
         
         if ( i1 .ne. 10 ) call zzrc( (inputno*100_4) + 1_4 )
         
         if ( ( .not. ieee_is_nan(rl1e) ) .or.                         &
     &        ( ieee_class(rl1e) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii1 .le. 0 ) ) call zzrc( (inputno*100_4) + 2_4 )
            
         if ( ieee_is_finite(rl2e) .or. (.not.ieee_is_negative(rl2e)) )&
     &        call zzrc( (inputno*100_4) + 3_4 )

         if ( c1 .ne. 'xlf' ) call zzrc((inputno*100_4) + 4_4 )

         if ( ( .not. ieee_is_nan(rl3e) ) .or.                         &
     &        ( ieee_class(rl3e) .ne. ieee_quiet_nan ) .or.            &
     &        ( ii3 .le. 0 ) ) call zzrc( (inputno*100_4) + 5_4 )         
         
         if ( i2 .ne. 11 ) call zzrc( (inputno*100_4) + 6_4 )

         if ( ieee_is_finite(rl4e) .or. ieee_is_negative(rl4e) )       &
     &        call zzrc( (inputno*100_4) + 7_4 )

         if ( c2 .ne. 'ibm' ) call zzrc( (inputno*100_4) + 8_4 )
         
         if ( ( .not. ieee_is_nan(rl5e) ) .or.                         &
     &        ( ieee_class(rl5e) .ne. ieee_signaling_nan ) .or.        &
     &        ( ii5 .ge. 0 ) ) call zzrc( (inputno*100_4) + 9_4 )          
         
         if ( ( .not. ieee_is_nan(rl6e) ) .or.                         &
     &        ( ieee_class(rl6e) .ne. ieee_quiet_nan ) .or.            &
     &        ( ii6 .ge. 0 ) ) call zzrc( (inputno*100_4) + 10_4 )

         if ( .not. precision_r6(3.14_16, rl7) )                       &
     &        call zzrc( (inputno*100_4) + 11_4 )
         
      end do

      if ( inputno .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if
      
      close(in)

      end
