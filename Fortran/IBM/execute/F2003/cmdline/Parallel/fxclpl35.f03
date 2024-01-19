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
!*  DESCRIPTION                : Call command line intrinsic routines through statement functions
!*                             : inside a parallel construct with args specified in a new clause
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

        DATA CmdLine/'fxclpl35 _______________________________________________________ ------'/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxclpl35

      USE MOD
      IMPLICIT NONE


      INTERFACE

        LOGICAL FUNCTION SF_GET_CMD(COMMAND, LENGTH, STATUS)
          character(2049)  :: COMMAND
          integer          :: LENGTH
          integer          :: STATUS
        END FUNCTION

        LOGICAL FUNCTION SF_GET_CMD_ARG(iCOUNT, NUMBER, VALUE, LENGTH, STATUS)
          INTEGER iCOUNT
          integer          :: NUMBER
          integer          :: LENGTH
          integer          :: STATUS
          character(2047)  :: VALUE
        END FUNCTION

        LOGICAL FUNCTION SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)
          integer          :: LENGTH
          integer          :: STATUS
          character(2047)  :: VALUE
        END FUNCTION

      END INTERFACE


      INTEGER  CMD_ARG_COUNT
      LOGICAL  GET_CMD
      LOGICAL  GET_CMD_ARG
      LOGICAL  GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer i

      CMD_ARG_COUNT() =  COMMAND_ARGUMENT_COUNT()
      GET_CMD(COMMAND, LENGTH, STATUS)                                                &
           =  SF_GET_CMD(COMMAND, LENGTH, STATUS)
      GET_CMD_ARG(NUMBER, VALUE, LENGTH, STATUS)                                      &
           =  SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT(), NUMBER, VALUE, LENGTH, STATUS)
      GET_ENV_VAR(VALUE, LENGTH, STATUS)                                              &
           =  SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)


    !$OMP  INDEPENDENT,NEW(COMMAND, LENGTH, STATUS, NUMBER, VALUE, ARGCOUNT)

       DO i = 1, 10

       IF (CMD_ARG_COUNT() .ne. 2 ) error stop 73


       IF (.not. GET_CMD(COMMAND, LENGTH, STATUS) )        error stop 74


       IF (.not. GET_CMD_ARG(NUMBER, VALUE, LENGTH, STATUS) )    error stop 75


       IF (.not. GET_ENV_VAR(VALUE, LENGTH, STATUS) )    error stop 76

      END DO


      END


      FUNCTION SF_GET_CMD(COMMAND, LENGTH, STATUS)

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
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        SF_GET_CMD = .false.
        ! error stop 64
      endif

      END FUNCTION

      FUNCTION SF_GET_CMD_ARG(CmdCount, NUMBER, VALUE, LENGTH, STATUS)

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

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          SF_GET_CMD_ARG = .false.
         ! error stop 65
        endif

      END DO

     END FUNCTION



      FUNCTION SF_GET_ENV_VAR(VALUE, LENGTH, STATUS)

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
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
         SF_GET_ENV_VAR = .false.
         ! error stop 66
      endif


      END FUNCTION



      INCLUDE 'cmdline.include'



