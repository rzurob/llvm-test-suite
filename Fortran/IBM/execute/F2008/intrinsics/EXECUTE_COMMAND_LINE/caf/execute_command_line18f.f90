! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-01-05
!*
!*  PRIMARY FUNCTIONS TESTED   : EXECUTE_COMMAND_LINE intrinsic
!*  SECONDARY FUNCTIONS TESTED : co-array
!*
!*  DESCRIPTION                :
!*
!*  In this tc the number of images has to be limited.
!*  Otherwise the program will attempt to create file names like:
!*  file: or file< (the later produces an error with the touch command)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
PROGRAM execute_command_line18f
      IMPLICIT NONE
      INTEGER, SAVE :: m[*]
      CHARACTER(100) :: cmd, msg
      INTEGER :: I, Istat, Icmd, me

      cmd = "ls file"
      msg = "default"
      Icmd = 0
      Istat = 0
      me = this_image()

      CALL EXECUTE_COMMAND_LINE("touch file"// achar(Iachar("0")+me))

      SYNC ALL

      IF (THIS_IMAGE() == 1) then
         DO i = 1, NUM_IMAGES()
            CALL EXECUTE_COMMAND_LINE(COMMAND=TRIM(cmd)//achar(Iachar("0")+i), CMDMSG=msg, EXITSTAT=Istat, CMDSTAT=Icmd)
            IF( msg   .NE. "default" ) ERROR STOP 10
            IF( Icmd  .NE.         0 ) ERROR STOP 11
            IF( Istat .NE.         0 ) ERROR STOP 12
         ENDDO
      ENDIF

      SYNC ALL

      CALL EXECUTE_COMMAND_LINE("rm file"// achar(Iachar("0")+me))

      SYNC ALL

      IF (THIS_IMAGE() == 1) then
         DO i = 1, NUM_IMAGES()
            CALL EXECUTE_COMMAND_LINE(COMMAND=TRIM(cmd)//achar(Iachar("0")+i), CMDMSG=msg, EXITSTAT=Istat, CMDSTAT=Icmd)
         ENDDO
      ENDIF
END PROGRAM execute_command_line18f
