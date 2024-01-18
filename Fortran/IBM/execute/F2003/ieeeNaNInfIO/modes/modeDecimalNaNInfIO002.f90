!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : modeDecimalNaNInfIO002.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 28, 2006
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
!*  Make sure the DECIMAL edit mode does not affect the Input/Output
!*  of IEEE exceptional specifications. This testcase covers format-directed
!*  I/O with decimal mode of COMMA.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      integer, parameter :: in = 11, out = 12
      
      real(4) :: rl1, rl2
      real(8) :: rl3, rl4
      real(16) :: rl5
      complex(4) :: cx1
      complex(8) :: cx2
      
      integer :: ios = 0
      
      open(in, file='modeDecimalNaNInfIO002.dat', action='read')
      open(out, file='modeDecimalNaNInfIO002.out', action='write')

      ! *****************************************************
      ! * READ THE FIRST LINE OF INPUT AND WRITE IT OUT
      ! *****************************************************

      ! reset variables
      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0; rl5 = 0.0
      cx1 = (0.0, 0.0); cx2 = (0.0, 0.0)

      read(in,'(dc,f6.0,f8.2, f10.10, f4.2, f3.3, f7.1, 2f6.2, f10.1)')&
     &     rl1, rl2, rl3, rl4, cx1, cx2, rl5
         
      write(out,'(dc, 9f9.1)') rl5, cx2, cx1, rl4, rl3, rl2, rl1
      write(out,'(dp, 9f9.1)') rl5, cx2, cx1, rl4, rl3, rl2, rl1

      ! *****************************************************
      ! * READ THE SECOND LINE OF INPUT AND WRITE IT OUT
      ! *****************************************************

      ! reset variables
      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0; rl5 = 0.0
      cx1 = (0.0, 0.0); cx2 = (0.0, 0.0)

      read(in,'(dc,f7.0, f8.2, f5.4, f10.2, f4.3, f9.1, f8.2, 2f6.1)') &
     &     rl1, rl2, rl3, rl4, cx1, cx2, rl5
         
      write(out,'(dc, 9f9.1)') rl5, cx2, cx1, rl4, rl3, rl2, rl1
      write(out,'(dp, 9f9.1)') rl5, cx2, cx1, rl4, rl3, rl2, rl1

      ! *****************************************************
      ! * READ THE THIRD LINE OF INPUT AND WRITE IT OUT
      ! *****************************************************

      ! reset variables
      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0; rl5 = 0.0
      cx1 = (0.0, 0.0); cx2 = (0.0, 0.0)

      read(in,'(dc,f7.0,f8.2,f5.5,f8.2,f3.3,f13.1,f9.2,f5.1,f12.0)')   &
     &     rl1, rl2, rl3, rl4, cx1, cx2, rl5
         
      write(out,'(dc, 9f9.1)') rl5, cx2, cx1, rl4, rl3, rl2, rl1
      write(out,'(dp, 9f9.1)') rl5, cx2, cx1, rl4, rl3, rl2, rl1

      ! *****************************************************
      ! * READ THE FOURTH LINE OF INPUT AND WRITE IT OUT
      ! *****************************************************

      ! reset variables
      rl1 = 0.0; rl2 = 0.0; rl3 = 0.0; rl4 = 0.0; rl5 = 0.0
      cx1 = (0.0, 0.0); cx2 = (0.0, 0.0)

      read(in,'(dc,f6.0,f5.2,f9.9,f8.2,f13.3,f6.1,f7.2,f6.2,f14.1)')   &
     &     rl1, rl2, rl3, rl4, cx1, cx2, rl5
         
      write(out,'(dc, 9f9.2)') rl5, cx2, cx1, rl4, rl3, rl2, rl1
      write(out,'(dp, 9f9.2)') rl5, cx2, cx1, rl4, rl3, rl2, rl1
      

      close(in)
      close(out)

      end
