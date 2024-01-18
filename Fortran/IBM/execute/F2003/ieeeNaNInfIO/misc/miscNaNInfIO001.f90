!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : miscNaNInfIO001.f
!*
!*  DATE                       : July 4, 2006
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
!*  Using the scale factor must not affec input / output of IEEE
!*  NaN and Infinity
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: in = 11, out = 12

      real(4)    :: rl1, rl2, rl3, rl4, rl5, rl6
      integer(4) :: rl1e, rl2e, rl3e, rl4e, rl5e, rl6e

      equivalence(rl1, rl1e); equivalence(rl2, rl2e)
      equivalence(rl3, rl3e); equivalence(rl4, rl4e)
      equivalence(rl5, rl5e); equivalence(rl6, rl6e)

      open(in,  file='miscNaNInfIO001.dat')
      open(out, file='miscNaNInfIO001.out')

      read(in, '(2P, f5.2, f8.2, f11.2, dc, f13.2, f7.2, f5.5)')      &
     &     rl1, rl2, rl3, rl4, rl5, rl6

      ! check the sign of rl5: should be +NaN(S)
      if ( rl5e .le. 0 ) error stop 1_4

      write(out,'(3P, 3F10.4, dc, 3F10.4)') rl1, rl2, rl3, rl4, rl5, rl6



      read(in, '(2P ,f9.2, f10.2, 2f7.3, f5.2, f5.5)')                &
     &     rl1, rl2, rl3, rl4, rl5, rl6

      ! check the sign of rl4 and rl5: should be +NaN(Q) and -NaN(Q)
      if ( ( rl4e .le. 0 ) .or. ( rl5e .ge. 0 ) ) error stop 2_4

      write(out, '(2P, 6F10.4)') rl1, rl2, rl3, rl4, rl5, rl6



      read(in, '(1P, f5.2, f6.2, f11.3, f8.2, f7.2, f6.5)')            &
     &     rl1, rl2, rl3, rl4, rl5, rl6

      ! check the sign of rl2, rl3, rl4, rl5, rl6 should be
      ! +NaN(Q), +NaN(Q), -NaN(Q), +NaN(Q) +NaN(S)
      if ( ( rl2e .le. 0 ) .or. ( rl3e .le. 0 ) .or. ( rl4e .ge. 0 )   &
     &     .or. ( rl5e .le. 0 ) .or. ( rl6e .le. 0 ) ) error stop 3_4

      write(out, '(3P, 6F10.4)') rl1, rl2, rl3, rl4, rl5, rl6



      read(in, '(2P, f5.5, f13.2, f7.2, f9.3, f6.2, f5.2)')            &
     &     rl1, rl2, rl3, rl4, rl5, rl6

      ! check the sign of rl2, rl3, rl4: should be
      ! +NaN(Q), +NaN(S), +NaN(Q)
      if ( ( rl2e .le. 0 ) .or. ( rl3e .le. 0 ) .or. ( rl4e .le. 0 ) ) &
     &     error stop 4_4

      write(out, '(1P, 6F10.4)') rl1, rl2, rl3, rl4, rl5, rl6

      close(in)
      close(out)

      end
