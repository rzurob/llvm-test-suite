!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 10, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : ERROR STOP in a CAF program
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : Test behaviour of the ERROR STOP statement
!*                               when there is a float point exception in
!*                               a CAF program.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      PROGRAM errorstop04
      INTEGER, SAVE :: acoarr(100)[*]
      INTEGER I, SELF, SUM
      REAL X, Y

      DO I=1, 100
        acoarr(I) = I
      END DO

      SELF = THIS_IMAGE()

      SYNC ALL
      if (self .eq. 1) then
        x = 0.0
        y = 1.0
	y = y / x
        ERROR STOP "from image #1!"
      else
        sum = 0
        do i=1, 100
          sum = sum + acoarr(I)
        end do
      end if

      SYNC ALL

      print *, "Should not have reached this point."
      END