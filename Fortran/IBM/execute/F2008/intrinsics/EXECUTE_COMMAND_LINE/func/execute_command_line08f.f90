! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-15
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : EXECUTE_COMMAND_LINE intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  EXECUTE_COMMAND_LINE(COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ])
!*
!*  Create a temporary file and write data in it. Rename the tmp file using EXECUTE_COMMAND_LINE('/bin/mv file1 file2')and read the data from the new file.
!*  Delete the new file once it is done with EXECUTE_COMMAND_LINE('/bin/rm file2'). Verify the data inside the program
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
PROGRAM execute_command_line08f
      IMPLICIT NONE
      CHARACTER(100) :: cmd, msg
      INTEGER :: I, J, Arr(100), tmp(2,100)
      INTEGER :: Icmd, Istat               ! var used for EXECUTE_COMMAND_LINE

      cmd = "mv file1 file2"
      msg = "default"
      Icmd = 0
      Istat = 0
      Arr = [(I, I=1,100)]

      open(unit=10, file='file1')
      DO I = 1, 100
          write(10, *) I, Arr(I)
      END DO
      close(10)

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDSTAT=Icmd, CMDMSG=msg, EXITSTAT=Istat)
      print*, msg
      print*, Icmd, Istat
      IF( msg   .NE. "default" ) ERROR STOP 10
      IF( Istat .NE.      Icmd ) ERROR STOP 11

      open(unit=20, file='file2')
      read (20, *) tmp
      DO I = 1, 100
         IF( ANY(tmp(:,I) .NE. I) ) ERROR STOP 20
      END DO
      close(20)

      CALL EXECUTE_COMMAND_LINE(COMMAND="rm file2", CMDSTAT=Icmd, CMDMSG=msg, EXITSTAT=Istat)
      print*, msg
      print*, Icmd, Istat
      IF( msg   .NE. "default" ) ERROR STOP 12
      IF( Istat .NE.      Icmd ) ERROR STOP 13

      print*, "End of the program: Normal termination"
END PROGRAM execute_command_line08f
