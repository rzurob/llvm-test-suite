!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
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
!*  Test having different letter cases for input of IEEE Infinity. A lower case
!*  letter should be equivalent to the corresponding upper-case letter
!*  in an IEEE exceptional specification. ( section 10.6.1.2 )
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use ieee_arithmetic

      implicit none
      real(4) :: rl1
      integer :: ios
      integer(4) :: i = 0  ! return code whose value corresponds
                           ! to the line of input causing the error.

      integer, parameter :: unit = 11

      open(unit, file='ieeeNaNInfBasic002.dat', action='read')

      do
         i = i+1

         rl1 = 0.0 ! reset rl1

         read(unit,'(f15.1)',iostat=ios) rl1

         if ( is_iostat_end(ios) ) exit

         if ( ieee_is_finite( rl1 ) ) call zzrc(i)

      end do

      if ( i .le. 1 ) then
         print *, "Error: No or bad input file"
         stop 1
      end if

      close(unit)


      end

