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
!*  Check for rank mismarch : pass array arguments instead of scalars
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
      INTEGER, PARAMETER :: N = 10

      CHARACTER(10)         :: cmd(5), msg(2)
      LOGICAL,DIMENSION(N)  :: flag = .True.
      INTEGER, ALLOCATABLE  :: Icmd(:)
      INTEGER, DIMENSION(2) :: Istat

      cmd = "echo test"
      msg = "pass!"
      ALLOCATE( Icmd(N) )

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd)
      CALL EXECUTE_COMMAND_LINE(COMMAND="echo test", CMDMSG=msg)
      CALL EXECUTE_COMMAND_LINE(COMMAND="echo test", EXITSTAT=Istat, CMDSTAT=Icmd)
      CALL EXECUTE_COMMAND_LINE(COMMAND="echo test", CMDMSG=msg, EXITSTAT=Istat, CMDSTAT=Icmd)

END PROGRAM execute_command_line02d
