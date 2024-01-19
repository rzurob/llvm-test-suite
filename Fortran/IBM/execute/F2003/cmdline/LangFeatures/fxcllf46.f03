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
!*  DESCRIPTION                : Invoke command line intrinsic routines by calling to external recursive
!*                             : subprogram within initial/final/step-expression of do loop
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

        character(513)   :: NAME
        logical          :: TRIM_NAME
        character(2049)  :: CmdLine

        COMMON /sargs/CmdLine, NAME, TRIM_NAME

        character(2049)  :: COMMAND
        integer          :: LENGTH
        integer          :: STATUS
        integer          :: NUMBER
        character(2047)  :: VALUE
        integer          :: ARGCOUNT

        COMMON /args/COMMAND, LENGTH, STATUS, NUMBER, VALUE, ARGCOUNT

      END MODULE


      BLOCK DATA

        character(513)   :: NAME
        logical          :: TRIM_NAME
        character(2049)  :: CmdLine

        COMMON /sargs/CmdLine, NAME, TRIM_NAME

        DATA CmdLine/'fxcllf46 1 a 2'/, NAME /'CmdLine   '/, TRIM_NAME /.true./
        ! '\a'  => ring bell

      END BLOCK DATA


      PROGRAM fxcllf46

      USE MOD
      IMPLICIT NONE

      INTERFACE
        RECURSIVE INTEGER FUNCTION F_GET_CMD()
        END FUNCTION

        RECURSIVE INTEGER FUNCTION F_GET_CMD_ARG(iCount)
        INTEGER iCount
        END FUNCTION

        RECURSIVE INTEGER FUNCTION F_GET_ENV_VAR()
        END FUNCTION
      END INTERFACE

      integer              :: i, j


      ! only execute once for each loop

      DO i = COMMAND_ARGUMENT_COUNT(), &
             COMMAND_ARGUMENT_COUNT(), &
             COMMAND_ARGUMENT_COUNT()
         IF (COMMAND_ARGUMENT_COUNT() .ne. 3 ) error stop 73
      END DO

      DO i = F_GET_CMD(), &
             F_GET_CMD(), &
             F_GET_CMD()
         j = F_GET_CMD()
      END DO

      DO i = F_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT()), &
             F_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT()), &
             F_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT())
         j =F_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT())
      END DO

      DO i = F_GET_ENV_VAR(),  &
             F_GET_ENV_VAR(),  &
             F_GET_ENV_VAR()
         j =F_GET_ENV_VAR()
      END DO





      END


      RECURSIVE FUNCTION F_GET_CMD()

      USE MOD

      INTEGER F_GET_CMD
      INTEGER, SAVE ::  Num /3/

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j


      IF( Num .ne. 1) THEN
          Num = Num - 1
          F_GET_CMD = F_GET_CMD()
      ELSE

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM( COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))       .or. &
           (STATUS .ne. 0) )                           &
      then
         error stop 64
      endif

      END IF

      F_GET_CMD = 3

      END FUNCTION


      RECURSIVE FUNCTION F_GET_CMD_ARG(CmdCount)

      USE MOD

      INTEGER  F_GET_CMD_ARG
      INTEGER, SAVE ::  Num /3/

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      F_GET_CMD_ARG = 3

      IF( Num .ne. 1) THEN
          Num = Num - 1
          F_GET_CMD_ARG = F_GET_CMD_ARG(CmdCount)
      ELSE

      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, F_GET_CMD_ARG )
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE)    .ne. TRIM(Argument))       .or. &
             (LENGTH         .ne. LEN(TRIM(Argument)))  .or. &
             (F_GET_CMD_ARG  .gt. 0) )                       &
        then
          error stop 65
        endif

        F_GET_CMD_ARG = 3

      END DO

      END IF

      END FUNCTION



      RECURSIVE FUNCTION F_GET_ENV_VAR()

      USE MOD

      INTEGER, SAVE        ::  Num /3/
      INTEGER      :: F_GET_ENV_VAR

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j


      IF( Num .ne. 1) THEN
        Num = Num - 1
        F_GET_ENV_VAR = F_GET_ENV_VAR()
      ELSE

      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      END IF

      F_GET_ENV_VAR  = 3

      END FUNCTION





      INCLUDE 'cmdline.include'
