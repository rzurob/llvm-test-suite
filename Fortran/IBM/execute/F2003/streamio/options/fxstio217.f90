!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 2003
!*
!*  PRIMARY FUNCTIONS TESTED   : OPEN, WRITE
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Test -qxlf90=nosignedzero option in
!*				 Stream Access output
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  04/01/03   BC     Initial version
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  program fxstio217

     implicit none
     integer  ios, pos
     integer, parameter   :: N = 10
     real                 :: r4_out
     real*8               :: r8_out
     real*16              :: r16_out
     complex              :: x8_out
     complex*16           :: x16_out
     complex*32           :: x32_out


!**********************************************************
!       Allocation, Association & Initialization          *
!**********************************************************

     r4_out = -0.0
     r8_out = -0.0D0 * huge(r8_out)
     r16_out = -0.0 * huge(r16_out)
     x8_out = (-0.00001, +0.00001)
     x16_out = (-0.0, +0.0)
     x32_out = (-0.0000Q-24, -0.00001Q-30)


!**********************************************************
!      Writing to the file with Stream access             *
!**********************************************************

     OPEN(1, FILE='fxstio217.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, ACTION='write')

     WRITE(1, FMT='(F10.5, D17.9, Q20.13)', IOSTAT=ios, ERR=91) &
    &      r4_out, r8_out, r16_out

     WRITE(1, FMT='(E10.3, F0.1, 2D19.11, 2H  ,2Q15.7)', IOSTAT=ios, ERR=91) &
    &      x8_out, x16_out, x32_out

     WRITE(1, FMT='(F7.1, F10.5, 2H  , F5.2)', IOSTAT=ios, ERR=91) &
    &        -0.000, +0.0000D-23, -0.001Q-12


     CLOSE(1)

     return

90   print *, "Error while openning the file: IOSTAT = ", ios
     error stop 90
91   print *, "Error while writing to the file: IOSTAT = ", ios
     error stop 91

   end program

