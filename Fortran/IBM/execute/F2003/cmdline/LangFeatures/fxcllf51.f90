! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf51 -1 -2 -3|fxcllf51 -1 -2 -3|fxcllf51 -1 -2 -3"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf51
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf51.f
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
!*  DESCRIPTION                : 1. Invoke command line procedures in statement functions which are called
!*                             : from recursive int./ext. subs
!*                             : 2. Test "|" on command line.
!*                             : Note: with "|", and the way using $CmdLine we will get the whole line
!*                             : "fxcllf51 -1 -2 -3|fxcllf51 -1 -2 -3|fxcllf51 -1 -2 -3" but not "fxcllf51 -1 -2 -3"
!*                             : by get_command
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

        DATA CmdLine/'fxcllf51 -1 -2 -3|fxcllf51 -1 -2 -3|fxcllf51 -1 -2 -3'/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf51

      USE MOD
      IMPLICIT NONE


      INTERFACE

        INTEGER FUNCTION SF_GET_CMD(COMMAND, LENGTH, STATUS)
          character(2049)  :: COMMAND
          integer          :: LENGTH
          integer          :: STATUS
        END FUNCTION

        INTEGER FUNCTION SF_GET_CMD_ARG(iCOUNT, NUMBER, VALUE, LENGTH, STATUS)
          INTEGER iCOUNT
          integer          :: NUMBER
          integer          :: LENGTH
          integer          :: STATUS
          character(2047)  :: VALUE
        END FUNCTION

        INTEGER FUNCTION SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)
          integer          :: LENGTH
          integer          :: STATUS
          character(2047)  :: VALUE
        END FUNCTION

      END INTERFACE


      INTEGER  CMD_ARG_COUNT
      INTEGER  GET_CMD
      INTEGER  GET_CMD_ARG
      INTEGER  GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer i

      CMD_ARG_COUNT() =  COMMAND_ARGUMENT_COUNT() + &
                         COMMAND_ARGUMENT_COUNT()+ &
                         COMMAND_ARGUMENT_COUNT()

      GET_CMD(COMMAND, LENGTH, STATUS)              &
           =  SF_GET_CMD(COMMAND, LENGTH, STATUS) + &
              SF_GET_CMD(COMMAND, LENGTH, STATUS) + &
              SF_GET_CMD(COMMAND, LENGTH, STATUS)


      GET_CMD_ARG(NUMBER, VALUE, LENGTH, STATUS)                                        &
           =  SF_GET_CMD_ARG(3, NUMBER, VALUE, LENGTH, STATUS) + &
              SF_GET_CMD_ARG(3, NUMBER, VALUE, LENGTH, STATUS) + &
              SF_GET_CMD_ARG(3, NUMBER, VALUE, LENGTH, STATUS)

      GET_ENV_VAR(VALUE, LENGTH, STATUS)               &
           =  SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)  + &
              SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)  + &
              SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)


       CALL INT_SUB

       CALL EXT_SUB


       CONTAINS

       RECURSIVE SUBROUTINE INT_SUB
       INTEGER, SAVE :: Recursive /4/

       Recursive =Recursive - 1

       IF (Recursive .ne. 1 ) THEN
         CALL INT_SUB
       ELSE

       ! IF (CMD_ARG_COUNT() .ne. 9) error stop 73   ! it will be 9


       IF (GET_CMD(COMMAND, LENGTH, STATUS) .ne. 0 )           error stop 74


       IF (GET_CMD_ARG(NUMBER, VALUE, LENGTH, STATUS) .ne. 0)  error stop 75


       IF (GET_ENV_VAR(VALUE, LENGTH, STATUS) .ne. 0)          error stop 76

       END IF

      END SUBROUTINE


      END


      RECURSIVE SUBROUTINE EXT_SUB

      USE MOD
      IMPLICIT NONE


      INTERFACE

        INTEGER FUNCTION SF_GET_CMD(COMMAND, LENGTH, STATUS)
          character(2049)  :: COMMAND
          integer          :: LENGTH
          integer          :: STATUS
        END FUNCTION

        INTEGER FUNCTION SF_GET_CMD_ARG(iCOUNT, NUMBER, VALUE, LENGTH, STATUS)
          INTEGER iCOUNT
          integer          :: NUMBER
          integer          :: LENGTH
          integer          :: STATUS
          character(2047)  :: VALUE
        END FUNCTION

        INTEGER FUNCTION SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)
          integer          :: LENGTH
          integer          :: STATUS
          character(2047)  :: VALUE
        END FUNCTION

      END INTERFACE


      INTEGER  CMD_ARG_COUNT
      INTEGER  GET_CMD
      INTEGER  GET_CMD_ARG
      INTEGER  GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer i

      INTEGER, SAVE :: Recursive /4/


      CMD_ARG_COUNT() =  COMMAND_ARGUMENT_COUNT() + &
                         COMMAND_ARGUMENT_COUNT()+ &
                         COMMAND_ARGUMENT_COUNT()

      GET_CMD(COMMAND, LENGTH, STATUS)              &
           =  SF_GET_CMD(COMMAND, LENGTH, STATUS) + &
              SF_GET_CMD(COMMAND, LENGTH, STATUS) + &
              SF_GET_CMD(COMMAND, LENGTH, STATUS)


      GET_CMD_ARG(NUMBER, VALUE, LENGTH, STATUS)                                        &
           =  SF_GET_CMD_ARG(3, NUMBER, VALUE, LENGTH, STATUS) + &
              SF_GET_CMD_ARG(3, NUMBER, VALUE, LENGTH, STATUS) + &
              SF_GET_CMD_ARG(3, NUMBER, VALUE, LENGTH, STATUS)

      GET_ENV_VAR(VALUE, LENGTH, STATUS)               &
           =  SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)  + &
              SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)  + &
              SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)



      Recursive =Recursive - 1

      IF (Recursive .ne. 1 ) THEN
         CALL EXT_SUB
      ELSE

      ! IF (CMD_ARG_COUNT() .ne.9 ) error stop 73  ! It'll be 9

       IF (GET_CMD(COMMAND, LENGTH, STATUS) .ne. 0 )           error stop 74


       IF (GET_CMD_ARG(NUMBER, VALUE, LENGTH, STATUS) .ne. 0)  error stop 75


       IF (GET_ENV_VAR(VALUE, LENGTH, STATUS) .ne. 0)          error stop 76


      END IF

      END SUBROUTINE



      FUNCTION SF_GET_CMD(COMMAND, LENGTH, STATUS)

      USE MOD

      INTEGER SF_GET_CMD

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
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        SF_GET_CMD = 1
        ! error stop 64
      endif

      END FUNCTION

      FUNCTION SF_GET_CMD_ARG(CmdCount, NUMBER, VALUE, LENGTH, STATUS)

      USE MOD

      INTEGER SF_GET_CMD_ARG

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

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          SF_GET_CMD_ARG = 1
         ! error stop 65
        endif

      END DO

     END FUNCTION



      FUNCTION SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)

      USE MOD

      INTEGER SF_GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_ENV_VAR = 0
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
         SF_GET_ENV_VAR = 1
         ! error stop 66
      endif


      END FUNCTION



      INCLUDE 'cmdline.include'



