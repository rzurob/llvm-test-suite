! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf49 1 a 2"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf49
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf49.f
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
!*  DESCRIPTION                : Invoke command line procedures through
!*                             : various character expressions.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

        character(513)   :: NAME
        logical          :: TRIM_NAME
        character(2049)  :: CmdLine, EnvCmdLine

        COMMON /sargs/CmdLine, NAME, TRIM_NAME

      END MODULE


      BLOCK DATA

        character(513)   :: NAME
        logical          :: TRIM_NAME
        character(2049)  :: CmdLine, EnvCmdLine

        COMMON /sargs/CmdLine, NAME, TRIM_NAME

        DATA CmdLine/'fxcllf49 1 a 2'/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf49

      USE MOD
      IMPLICIT NONE


      INTERFACE

        character(2049) FUNCTION SF_GET_CMD(COMMAND, LENGTH, STATUS)
          character(2049)  :: COMMAND
          integer          :: LENGTH
          integer          :: STATUS
        END FUNCTION

        character(2049) FUNCTION SF_GET_CMD_ARG(iCOUNT, NUMBER, VALUE, LENGTH, STATUS)
          INTEGER iCOUNT
          integer          :: NUMBER
          integer          :: LENGTH
          integer          :: STATUS
          character(2047)  :: VALUE
        END FUNCTION

        character(2049) FUNCTION SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)
          integer          :: LENGTH
          integer          :: STATUS
          character(2047)  :: VALUE
        END FUNCTION

      END INTERFACE


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      character(2049)  :: TempStr



       ! Various string expressions

       IF (COMMAND_ARGUMENT_COUNT() .ne. 3 ) error stop 73

       TempStr = SF_GET_CMD(COMMAND, LENGTH, STATUS) // SF_GET_CMD(COMMAND, LENGTH, STATUS)

       TempStr = SF_GET_CMD(COMMAND, LENGTH, STATUS) // SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)

       TempStr = SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT(), NUMBER, VALUE, LENGTH, STATUS)

       TempStr = SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT(), NUMBER, VALUE, LENGTH, STATUS)(1:2044)

       TempStr(1:COMMAND_ARGUMENT_COUNT()) =  &
             SF_GET_CMD(COMMAND, LENGTH, STATUS)(1:COMMAND_ARGUMENT_COUNT())

       TempStr(1:COMMAND_ARGUMENT_COUNT()) =                                                       &
                       SF_GET_CMD(COMMAND, LENGTH, STATUS) //                                      &
                       SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT(), NUMBER, VALUE, LENGTH, STATUS)  // &
                       SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)
      END


      FUNCTION SF_GET_CMD(COMMAND, LENGTH, STATUS)

      USE MOD

      character(2049) SF_GET_CMD

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT


      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_CMD = ' '

      call GET_COMMAND(SF_GET_CMD, LENGTH, STATUS)
      if ( (TRIM(SF_GET_CMD) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        SF_GET_CMD = 'Err'
        error stop 64
      endif

      END FUNCTION

      FUNCTION SF_GET_CMD_ARG(CmdCount, NUMBER, VALUE, LENGTH, STATUS)

      USE MOD

      character(2049) SF_GET_CMD_ARG

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_CMD_ARG = ' '

      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, SF_GET_CMD_ARG, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(SF_GET_CMD_ARG) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          SF_GET_CMD_ARG = 'Err'
          error stop 65
        endif

      END DO

     END FUNCTION



      FUNCTION SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)

      USE MOD

      character(2049) SF_GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_ENV_VAR = ' '
      call GET_ENVIRONMENT_VARIABLE(NAME, SF_GET_ENV_VAR, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(SF_GET_ENV_VAR) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
         SF_GET_ENV_VAR = 'Err'
         error stop 66
      endif


      END FUNCTION



      INCLUDE 'cmdline.include'



