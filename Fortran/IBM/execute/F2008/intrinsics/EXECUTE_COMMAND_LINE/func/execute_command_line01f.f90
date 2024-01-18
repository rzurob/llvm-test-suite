! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-12-15
!*
!*  PRIMARY FUNCTIONS TESTED   : EXECUTE_COMMAND_LINE intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  EXECUTE_COMMAND_LINE(COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ])
!*
!*  Test that no error message is emitted when:
!*    *  WAIT, EXITSTAT, CMDSTAT or CMDMSG are omitted or present
!*        (24 combinaitions !)
!*    *  and that CMDSTAT is never set to -1
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
PROGRAM execute_command_line01f
      IMPLICIT NONE
      CHARACTER(100) :: cmd, msg
      INTEGER :: Istat, Icmd
      LOGICAL :: Flag = .True.

      cmd = "echo test command line execution"
      msg = "success!"
      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd)

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, WAIT=Flag)
      IF( Flag .NEQV. .True. )   ERROR STOP 10

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDMSG=msg)
      IF( msg .NE. "success!" )  ERROR STOP 11

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, EXITSTAT=Istat)
      IF( Istat .NE.  0 )        ERROR STOP 12

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDSTAT=Icmd)
      IF( Icmd .EQ. -1 )         ERROR STOP 13

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, EXITSTAT=Istat, CMDSTAT=Icmd)
      IF( Istat .NE.  0 )       ERROR STOP 14
      IF( Icmd  .EQ. -1 )       ERROR STOP 15

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, EXITSTAT=Istat, CMDMSG=msg)
      IF( Istat .NE.  0 )       ERROR STOP 16
      IF( msg .NE. "success!" ) ERROR STOP 17

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, EXITSTAT=Istat, WAIT=Flag)
      IF( Istat .NE.  0 )       ERROR STOP 18
      IF( Flag .NEQV. .True. )  ERROR STOP 19

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDSTAT=Icmd, CMDMSG=msg)
      IF( msg .NE. "success!" ) ERROR STOP 20
      IF( Icmd  .EQ. -1 )       ERROR STOP 21

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDSTAT=Icmd, WAIT=Flag)
      IF( Flag .NEQV. .True. )  ERROR STOP 22
      IF( Icmd  .EQ. -1 )       ERROR STOP 23

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDMSG=msg, WAIT=Flag)
      IF( msg .NE. "success!" ) ERROR STOP 24
      IF( Flag .NEQV. .True. )  ERROR STOP 25

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDMSG=msg, EXITSTAT=Istat, CMDSTAT=Icmd)
      IF( msg .NE. "success!" ) ERROR STOP 26
      IF( Icmd .EQ. -1 )        ERROR STOP 27
      IF( Istat .NE.  0 )       ERROR STOP 28

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDMSG=msg, EXITSTAT=Istat, WAIT=Flag)
      IF( Istat .NE.  0 )       ERROR STOP 29
      IF( Flag .NEQV. .True. )  ERROR STOP 30
      IF( msg .NE. "success!" ) ERROR STOP 31

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDMSG=msg, WAIT=Flag, CMDSTAT=Icmd)
      IF( Flag .NEQV. .True. )  ERROR STOP 32
      IF( msg .NE. "success!" ) ERROR STOP 33
      IF( Icmd .EQ. -1 )        ERROR STOP 34

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, EXITSTAT=Istat, CMDSTAT=Icmd, WAIT=Flag)
      IF( Istat .NE.  0 )       ERROR STOP 35
      IF( Flag .NEQV. .True. )  ERROR STOP 36
      IF( msg .NE. "success!" ) ERROR STOP 37
      IF( Icmd .EQ. -1 )        ERROR STOP 38

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDMSG=msg, CMDSTAT=Icmd, WAIT=Flag)
      IF( Flag .NEQV. .True. )  ERROR STOP 39
      IF( msg .NE. "success!" ) ERROR STOP 40
      IF( Icmd .EQ. -1 )        ERROR STOP 41

      CALL EXECUTE_COMMAND_LINE(COMMAND=cmd, CMDMSG=msg, EXITSTAT=Istat, CMDSTAT=Icmd, WAIT=Flag)
      IF( Flag .NEQV. .True. )  ERROR STOP 42
      IF( msg .NE. "success!" ) ERROR STOP 43
      IF( Icmd .EQ. -1 )        ERROR STOP 44
      IF( Istat .NE.  0 )       ERROR STOP 45

END PROGRAM execute_command_line01f
