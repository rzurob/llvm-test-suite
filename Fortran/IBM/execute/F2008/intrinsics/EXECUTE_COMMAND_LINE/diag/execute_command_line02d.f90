! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : execute_command_line02d.f
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
!*  Test the compilation fails if:
!*
!*  * COMMAND/CMDMSG not of character type
!*  * EXITSTAT/CMDSTAT not integer
!*  * WAIT not logical
!*
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
PROGRAM execute_command_line02d
      IMPLICIT NONE
      REAL          :: rstat, rcmd
      INTEGER       :: icmd, imsg 
      CHARACTER(10) :: cmd , cwait, cstat

      cmd = "echo test" 
      CALL EXECUTE_COMMAND_LINE(COMMAND=icmd)                  !  fails 
      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd,WAIT=cwait)        !  fails 
      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd,CMDMSG=imsg)       !  fails 
      CALL EXECUTE_COMMAND_LINE(COMMAND=icmd,CMDMSG=imsg)      !  fails 
      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd,CMDSTAT=cstat)     !  fails 
      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd,EXITSTAT=rstat)    !  fails 
END PROGRAM execute_command_line02d
