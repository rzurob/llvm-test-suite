! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : execute_command_line02f.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-12-15
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : EXECUTE_COMMAND_LINE intrinsic
!*                             :
!*  SECONDARY FUNCTIONS TESTED :  
!*                                
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                :  
!*
!*  EXECUTE_COMMAND_LINE(COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ])
!*
!*  CMDSTAT =-2 when WAIT is set to .False. and no error condition occurs
!*  both with runtime and compile time values 
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
PROGRAM execute_command_line02f
      IMPLICIT NONE
      CHARACTER(100) :: cmd, msg 
      INTEGER :: Icmd = 0, Istat = 0 
      LOGICAL :: Flag = .False. 

      cmd = "echo test"
      msg = "default"

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, WAIT=Flag, CMDSTAT=Icmd)    
      IF( Flag  .NEQV. .False. ) ERROR STOP 11 
      IF( Icmd  .NE.        -2 ) ERROR STOP 12

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, WAIT=Flag, CMDSTAT=Icmd, CMDMSG=msg)    
      IF( Flag .NEQV.  .False. ) ERROR STOP 13 
      IF( msg   .EQ. "default" ) ERROR STOP 14
      IF( Icmd  .NE.        -2 ) ERROR STOP 15

      CALL EXECUTE_COMMAND_LINE(COMMAND="echo test", CMDMSG=msg, EXITSTAT=Istat, WAIT=Flag, CMDSTAT=Icmd)   
      IF( Flag .NEQV.  .False. ) ERROR STOP 16 
      IF( msg   .EQ. "default" ) ERROR STOP 17
      IF( Istat .NE.         0 ) ERROR STOP 18
      IF( Icmd  .NE.        -2 ) ERROR STOP 19

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDMSG=msg, EXITSTAT=Istat, WAIT=.False., CMDSTAT=Icmd)   
      IF( msg   .EQ. "default" ) ERROR STOP 20
      IF( Istat .NE.         0 ) ERROR STOP 21
      IF( Icmd  .NE.        -2 ) ERROR STOP 22

      CALL EXECUTE_COMMAND_LINE(COMMAND="echo test", CMDMSG=msg, CMDSTAT=Icmd, WAIT=Flag)   
      IF( Flag .NEQV.  .False. ) ERROR STOP 23 
      IF( msg   .EQ. "default" ) ERROR STOP 24
      IF( Icmd  .NE.        -2 ) ERROR STOP 25

      CALL EXECUTE_COMMAND_LINE(COMMAND="echo test", CMDSTAT=Icmd, WAIT=.False.)   
      IF( Icmd  .NE.        -2 ) ERROR STOP 27

      CALL EXECUTE_COMMAND_LINE(COMMAND="echo test", CMDSTAT=Icmd, EXITSTAT=Istat, WAIT=.False.)   
      IF( Istat .NE.         0 ) ERROR STOP 28
      IF( Icmd  .NE.        -2 ) ERROR STOP 29
print*, 'Normal termination of the program' 
END PROGRAM execute_command_line02f
