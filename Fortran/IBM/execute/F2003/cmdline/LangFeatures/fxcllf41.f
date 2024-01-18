! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf41 , , , , ,"
! %COMPOPTS:  -qfixed=132
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf41
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf41.f
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
!*  DESCRIPTION                : Use procedure name as part of statement function name
!*                             : like "call cammand_argument_count = cammand_argument_count()"
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

        DATA CmdLine/'fxcllf41 , , , , ,'/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf41

      USE MOD
      IMPLICIT NONE


      INTERFACE

        LOGICAL FUNCTION SF_GET_CMD()
        END FUNCTION

        LOGICAL FUNCTION SF_GET_CMD_ARG(iCount)
          INTEGER iCOUNT
        END FUNCTION

        LOGICAL FUNCTION SF_GET_ENV_VAR()
        END FUNCTION

      END INTERFACE


      INTEGER  CALL COMMAND_ARGUMENT_COUNT
      LOGICAL  CALL GET_COMMAND
      LOGICAL  CALL GET_COMMAND_ARGUMENT
      LOGICAL  CALL GET_ENVIRONMENT_VARIABL


      CALL COMMAND_ARGUMENT_COUNT() =  COMMAND_ARGUMENT_COUNT()
      CALL GET_COMMAND()       =  SF_GET_CMD()
      CALL GET_COMMAND_ARGUMENT()   =  SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT())
      CALL GET_ENVIRONMENT_VARIABL()   =  SF_GET_ENV_VAR()



      IF (CALL COMMAND_ARGUMENT_COUNT() .ne. 5 ) ERROR STOP 73

      IF (.not. CALL GET_COMMAND () )        ERROR STOP 74

      IF (.not. CALL GET_COMMAND_ARGUMENT() )    ERROR STOP 75

      IF (.not. CALL GET_ENVIRONMENT_VARIABL() )    ERROR STOP 76



      END


      FUNCTION SF_GET_CMD()

      USE MOD

      LOGICAL SF_GET_CMD

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT


      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_CMD = .true.

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or.
     C     (LENGTH .ne. LEN(TRIM(CmdLine)))    .or.
     C     (STATUS .ne. 0) )
     Cthen
        SF_GET_CMD = .false.
        ! error stop 64
      endif

      END FUNCTION


      FUNCTION SF_GET_CMD_ARG(CmdCount)

      USE MOD

      LOGICAL SF_GET_CMD_ARG

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_CMD_ARG = .true.

      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or.
     C       (LENGTH      .ne. LEN(TRIM(Argument)))  .or.
     C       (STATUS      .ne. 0) )
     C  then
          SF_GET_CMD_ARG = .false.
         ! error stop 65
        endif

      END DO

      END FUNCTION



      FUNCTION SF_GET_ENV_VAR()

      USE MOD

      LOGICAL SF_GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_ENV_VAR = .true.
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))   .or.
     C      (LENGTH .ne. LEN(TRIM(CmdLine)))  .or.
     C      (STATUS .ne. 0))
     Cthen
         SF_GET_ENV_VAR = .false.
         ! error stop 66
      endif


      END FUNCTION



      INCLUDE 'cmdline.include'




