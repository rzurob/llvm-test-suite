! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct 1, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   	: COMMAND_ARGUMENT_COUNT()
!*                            	: GET_COMMAND(COMMAND, LENGTH, STATUS)
!*                            	: GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
!*                             	: GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 252525
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Tests command line intrinsic routines by passing a long command line
!*                             : through a system call to imitate issuing a command directly on
!*                             : command line without involving a shell
!*                             : fxclms05.c forms a maximum length of a command with the form :
!*                             : fxclms050 OptionLength xx.......x
!*                             : Then the c program call a system call "execlp" to execute the formed
!*                             : command line.
!*                             : The detail explanation on the forming of a command line is in the c
!*                             : program "fxclms05.c"
!*                             : The maximum lengths (ARG_MAX) on various systems :
!*
!*                             : AIX:   /usr/include/sys/limits.h      24k = 24576
!*                             : MACOS: /usr/include/sys/syslimits.h   64k = 65536
!*                             : SLES:  /usr/include/linux/limits.h    128k= 131072
!*                             : RHEL:  /usr/include/linux/limits.h    128k= 131072
!*                             : Call chain:
!*                             : fxclms05.sh => fxclms05.c => fxclms050.f
!234567890123456789012345678901234567890123456789012345678901234567890

