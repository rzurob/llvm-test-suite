! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept 18, 2003
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
!*  DESCRIPTION                : Pass COMMAND_ARGUMENT_COUNT as arguments to other routines
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      PROGRAM fxclat30


      INTRINSIC COMMAND_ARGUMENT_COUNT
      INTRINSIC GET_COMMAND                 !Generic Name
      INTRINSIC GET_COMMAND_ARGUMENT        !Generic Name
      INTRINSIC GET_ENVIRONMENT_VARIABLE    !Generic Name

      character(2049)              :: CmdLine = 'fxclat30 111 2222 33333--== 254-24-345-3245'
      integer                      :: CmdCount, i
      character(2047),target       :: Argument, TNAME, COMMAND


     if ( ICOMMAND_ARGUMENT_COUNT(COMMAND_ARGUMENT_COUNT ) .ne. 4) &
      then
        error stop 63
      endif

     if ( JCOMMAND_ARGUMENT_COUNT(COMMAND_ARGUMENT_COUNT ) .ne. 4) &
      then
        error stop 64
      endif

     if ( KCOMMAND_ARGUMENT_COUNT(COMMAND_ARGUMENT_COUNT ) .ne. 4) &
      then
        error stop 65
      endif

      CONTAINS

        FUNCTION KCOMMAND_ARGUMENT_COUNT(FUN)
          INTEGER KCOMMAND_ARGUMENT_COUNT
          INTEGER, EXTERNAL :: FUN

          KCOMMAND_ARGUMENT_COUNT =   JCOMMAND_ARGUMENT_COUNT(FUN)

        END FUNCTION

      END

      INCLUDE 'cmdline.include'


      FUNCTION ICOMMAND_ARGUMENT_COUNT(FUN)
        INTEGER ICOMMAND_ARGUMENT_COUNT
        INTEGER FUN

        ICOMMAND_ARGUMENT_COUNT =  FUN()

      END FUNCTION

      FUNCTION JCOMMAND_ARGUMENT_COUNT(FUN)
        INTEGER JCOMMAND_ARGUMENT_COUNT
        INTEGER, EXTERNAL :: FUN

        JCOMMAND_ARGUMENT_COUNT =   ICOMMAND_ARGUMENT_COUNT(FUN)

      END FUNCTION




