!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : inputComplexInf002.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : June 8, 2006
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
!*  Test input of IEEE Infinity (positive & negative) for complex(4)/(8)/(16)
!*  with the E edit descriptor.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_arithmetic 
      implicit none

      complex(4)  :: cx1
      complex(8)  :: cx2
      complex(16) :: cx3
      real(4)     :: rl4r, rl4i ! rl4r holds the real part, rl4i is the imaginary.
      real(8)     :: rl8r, rl8i, rl6requiv, rl6iequiv
      real(16)    :: rl6r, rl6i
      integer     :: ios
      integer(4)  :: i = 0 ! return code whose least significant 3-digit
                           ! value correspond to the line of input causing the error.

      integer, parameter :: unit = 11

      equivalence(rl6iequiv, rl6i)
      equivalence(rl6requiv, rl6r) ! ieee intrinsics are not fully supported for
                                   ! extended reals, so we input to an extended
                                   ! real, and then use the real(8) equivalent
                                   ! for verification.


      ! open the file containing positive IEEE Infinity input values
      open(unit, file='cpx_inf_pos.dat', action='read')

      ! Read in the positive infinity values a complex(4) variable
      do
         i = i+1

         cx1 = (0.0, 0.0) ! reset cx1

         read(unit,'(2E15.1)',iostat=ios) cx1
         
         if ( is_iostat_end(ios) ) exit

         rl4r = real(cx1)
         rl4i = imag(cx1)

         if ( ieee_is_finite( rl4r ) .or.                   &
     &        ieee_is_finite( rl4i ) ) call zzrc(1000_4+i)

         if ( ieee_is_negative( rl4r ) .or.                   &
     &        ieee_is_negative( rl4i ) ) call zzrc(2000_4+i)
                  
      end do

      if ( i .le. 1 ) then
         print*, "Error: No or bad input file"
         stop 1
      end if
      
      rewind(unit) ! reposition the file to the beginning.
      i = 0

      ! Read in the positive infinity values to a complex(8) variable
      do
         i = i+1

         cx2 = (0.0, 0.0) ! reset cx2

         read(unit,'(2E15.1)',iostat=ios) cx2
         
         if ( is_iostat_end(ios) ) exit

         rl8r = dreal(cx2)
         rl8i = dimag(cx2)

         if ( ieee_is_finite( rl8r ) .or.                   &
     &        ieee_is_finite( rl8i ) ) call zzrc(3000_4+i)

         if ( ieee_is_negative( rl8r ) .or.                   &
     &        ieee_is_negative( rl8i ) ) call zzrc(4000_4+i)
                  
      end do

      if ( i .le. 1 ) then
         print*, "Error: No or bad input file"
         stop 1
      end if
      
      rewind(unit) ! reposition the file to the beginning.
      i = 0


      ! Read in the positive infinity values to a complex(16) variable
      do
         i = i+1

         cx3 = (0.0, 0.0) ! reset cx3

         read(unit,'(2E15.1)',iostat=ios) cx3
         
         if ( is_iostat_end(ios) ) exit

         rl6r = qreal(cx3)
         rl6i = qimag(cx3)

         if ( ieee_is_finite( rl6requiv ) .or.                   &
     &        ieee_is_finite( rl6iequiv ) ) call zzrc(5000_4+i)

         if ( ieee_is_negative( rl6requiv ) .or.                   &
     &        ieee_is_negative( rl6iequiv ) ) call zzrc(6000_4+i)

      end do

      if ( i .le. 1 ) then
         print*, "Error: No or bad input file"
         stop 1
      end if
      
      close(unit)
      i = 0


      ! open the file containing negative IEEE Infinity input values
      open(unit, file='cpx_inf_neg.dat', action='read')

      ! Read in the negative infinity values to a complex(4) variable
      do
         i = i+1

         cx1 = (0.0, 0.0) ! reset cx1

         read(unit,'(2E15.1)',iostat=ios) cx1
         
         if ( is_iostat_end(ios) ) exit

         rl4r = real(cx1)
         rl4i = imag(cx1)

         if ( ieee_is_finite( rl4r ) .or.                   &
     &        ieee_is_finite( rl4i ) ) call zzrc(7000_4+i)

         if ( .not. ( ieee_is_negative( rl4r ) .and.                   &
     &                ieee_is_negative( rl4i ) ) ) call zzrc(8000_4+i)
                  
      end do

      if ( i .le. 1 ) then
         print*, "Error: No or bad input file"
         stop 1
      end if
      
      rewind(unit) ! reposition the file to the beginning.
      i = 0

      ! Read in the negative infinity values to a complex(8) variable
      do
         i = i+1

         cx2 = (0.0, 0.0) ! reset cx2

         read(unit,'(2E15.1)',iostat=ios) cx2
         
         if ( is_iostat_end(ios) ) exit

         rl8r = dreal(cx2)
         rl8i = dimag(cx2)

         if ( ieee_is_finite( rl8r ) .or.                   &
     &        ieee_is_finite( rl8i ) ) call zzrc(9000_4+i)

         if ( .not. ( ieee_is_negative( rl8r ) .and.                   &
     &                ieee_is_negative( rl8i ) ) ) call zzrc(10000_4+i)
                  
      end do

      if ( i .le. 1 ) then
         print*, "Error: No or bad input file"
         stop 1
      end if
      
      rewind(unit) ! reposition the file to the beginning.
      i = 0


      ! Read in the negative infinity values to a complex(16) variable
      do
         i = i+1

         cx3 = (0.0, 0.0) ! reset cx3

         read(unit,'(2E15.1)',iostat=ios) cx3
         
         if ( is_iostat_end(ios) ) exit

         rl6r = qreal(cx3)
         rl6i = qimag(cx3)

         if ( ieee_is_finite( rl6requiv ) .or.                   &
     &        ieee_is_finite( rl6iequiv ) ) call zzrc(11000_4+i)

         if ( .not. ( ieee_is_negative( rl6requiv ) .and.                   &
     &                ieee_is_negative( rl6iequiv ) ) ) call zzrc(12000_4+i)

      end do

      if ( i .le. 1 ) then
         print*, "Error: No or bad input file"
         stop 1
      end if
      
      close(unit)


      end
