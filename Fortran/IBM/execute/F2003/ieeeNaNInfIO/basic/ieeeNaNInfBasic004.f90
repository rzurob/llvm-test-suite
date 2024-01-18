!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ieeeNaNInfBasic004.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 7, 2006
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
!*  Test having different acceptable formats of Signaling NaN as described
!*  in section 10.6.1.2 of the standard.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic 
      implicit none

      real(4) :: rl1
      integer :: ios
      integer(4) :: i = 0 ! return code whose value corresponds
                          ! to the line of input causing the error.

      integer, parameter :: unit = 11

      open(unit, file='real_nan_s.dat', action='read')

      do
         i = i+1

         rl1 = 0.0 ! reset rl1

         read(unit,'(f15.1)',iostat=ios) rl1
         
         if ( is_iostat_end(ios) ) exit

         if ( .not. ieee_is_nan( rl1 ) ) call zzrc(1000_4+i)

         if ( ieee_class( rl1 ) .ne. ieee_signaling_nan ) call zzrc(2000_4+i)
                  
      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      close(unit)


      end
      
