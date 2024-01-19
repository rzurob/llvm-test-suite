! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-01-05
!*
!*  PRIMARY FUNCTIONS TESTED   : EXECUTE_COMMAND_LINE intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  EXECUTE_COMMAND_LINE(COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ])
!*
!*  create a shell script and run it
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
PROGRAM execute_command_line12f


      CALL EXECUTE_COMMAND_LINE("echo '#!/usr/bin/ksh' > k.ksh")
      CALL EXECUTE_COMMAND_LINE("echo i=0 >> k.ksh")
      CALL EXECUTE_COMMAND_LINE("echo while [ \\$i -lt 100 ] >> k.ksh")
      CALL EXECUTE_COMMAND_LINE("echo do  >> k.ksh")
      CALL EXECUTE_COMMAND_LINE("echo \\(\\( i = \\$i + 1 \\)\\)  >> k.ksh")
      CALL EXECUTE_COMMAND_LINE("echo echo \\$i >> k.ksh")
      CALL EXECUTE_COMMAND_LINE("echo done  >> k.ksh")

      CALL EXECUTE_COMMAND_LINE("chmod 777 k.ksh")
      CALL EXECUTE_COMMAND_LINE("./k.ksh")

      print*, "End of the program: Normal termination"
END PROGRAM execute_command_line12f
