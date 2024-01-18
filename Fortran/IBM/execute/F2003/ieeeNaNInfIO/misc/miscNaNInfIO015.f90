!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : miscNaNInfIO015.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : July 26, 2006
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
!*  Testing the value of SIZE= specifier when doing non-advancing input 
!*  with IEEE exceptional specification.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      
      use, intrinsic :: ieee_arithmetic
      implicit none

      real(4) :: rl1, rl2
      
      integer(4) :: ii1, ii2
      
      integer :: sz

      integer, parameter :: in = 11

      equivalence(rl1, ii1)
      equivalence(rl2, ii2)
      
      open(unit=in, file='miscNaNInfIO015.dat', action='read')
      
      read(in, '(f7.2, es9.3)', advance='no', size=sz) rl1, rl2
      
      if ( sz .ne. 16 ) error stop 1_4

      if ( (.not. ieee_is_nan(rl1)) .or.                              &
     &     ( ieee_class(rl1) .ne. ieee_quiet_nan ) .or.               &
     &     ( ii1 .le. 0 ) ) error stop 2_4

      if ( ieee_is_finite(rl2) .or. .not. ieee_is_negative(rl2) ) then
         error stop 3_4
      end if

      
      read(in, '(g7.3)', advance='no', pad='yes', eor=100, size=sz) rl1
      
 100  if ( sz .ne. 4 ) error stop 4_4
      if ( ieee_is_finite(rl1) .or. ieee_is_negative(rl1) ) error stop 5_4


      read(in,'(f7.2, ru, dc, f12.10)',advance='no',eor=200, size=sz)  &
     &     rl1, rl2
      
 200  if ( sz .ne. 18 ) error stop 6_4

      if ( (.not. ieee_is_nan(rl1)) .or.                              &
     &     ( ieee_class(rl1) .ne. ieee_signaling_nan ) .or.           &
     &     ( ii1 .ge. 0 ) ) error stop 7_4
      
      if ( (.not. ieee_is_nan(rl2)) .or.                              &
     &     ( ieee_class(rl2) .ne. ieee_quiet_nan ) .or.               &
     &     ( ii2 .ge. 0 ) ) error stop 8_4
      

      end
