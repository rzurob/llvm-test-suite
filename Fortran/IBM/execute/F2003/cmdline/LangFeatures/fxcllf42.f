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
!*  DESCRIPTION                : Use procedure name as as part of identifiers
!*                             : like " INTEGER call cammand_argument_count "
!*                             : in the fixed form
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

        character(513)   :: NAME
        logical          :: TRIM_NAME
        character(2049)  :: CmdLine

        COMMON /sargs/CmdLine, NAME, TRIM_NAME

      END MODULE


      BLOCK DATA

        character(513)   :: NAME
        logical          :: TRIM_NAME
        character(2049)  :: CmdLine

        COMMON /sargs/CmdLine, NAME, TRIM_NAME

        DATA CmdLine/'fxcllf42 . ... . ..'/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf42

      USE MOD
      IMPLICIT NONE


      INTERFACE

        INTEGER FUNCTION SF_GET_CMD()
        END FUNCTION

        INTEGER FUNCTION SF_GET_CMD_ARG(iCount)
          INTEGER iCOUNT
        END FUNCTION

        INTEGER FUNCTION SF_GET_ENV_VAR()
        END FUNCTION

      END INTERFACE


      INTEGER  CALL COMMAND_ARGUMENT_COUNT
      INTEGER  CALL GET_COMMAND
      INTEGER  CALL GET_COMMAND_ARGUMENT
      INTEGER  CALL GET_ENVIRONMENT_VARIABL


      CALL COMMAND_ARGUMENT_COUNT =  COMMAND_ARGUMENT_COUNT()
      CALL GET_COMMAND      =  SF_GET_CMD()
      CALL GET_COMMAND_ARGUMENT   =  SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT())
      CALL GET_ENVIRONMENT_VARIABL   =  SF_GET_ENV_VAR()



      IF (CALL COMMAND_ARGUMENT_COUNT .ne. 4 ) ERROR STOP 73

      IF ( CALL GET_COMMAND .ne. 0 )        ERROR STOP 74

      IF ( CALL GET_COMMAND_ARGUMENT .ne. 0  )    ERROR STOP 75

      IF ( CALL GET_ENVIRONMENT_VARIABL .ne. 0  )    ERROR STOP 76



      END


      FUNCTION SF_GET_CMD()

      USE MOD

      INTEGER  SF_GET_CMD

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT


      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_CMD = 0

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or.
     C     (LENGTH .ne. LEN(TRIM(CmdLine)))    .or.
     C     (STATUS .ne. 0) )
     Cthen
        SF_GET_CMD = STATUS + 1
        ! error stop 64
      endif

      END FUNCTION


      FUNCTION SF_GET_CMD_ARG(CmdCount)

      USE MOD

      INTEGER  SF_GET_CMD_ARG

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_CMD_ARG = 0

      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or.
     C       (LENGTH      .ne. LEN(TRIM(Argument)))  .or.
     C       (STATUS      .ne. 0) )
     C  then
          SF_GET_CMD_ARG = STATUS + 1
         ! error stop 65
        endif

      END DO

      END FUNCTION



      FUNCTION SF_GET_ENV_VAR()

      USE MOD

      INTEGER  SF_GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_ENV_VAR = 0.
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))   .or.
     C      (LENGTH .ne. LEN(TRIM(CmdLine)))  .or.
     C      (STATUS .ne. 0))
     Cthen
         SF_GET_ENV_VAR = STATUS + 1
         ! error stop 66
      endif


      END FUNCTION



      INCLUDE 'cmdline.include'


