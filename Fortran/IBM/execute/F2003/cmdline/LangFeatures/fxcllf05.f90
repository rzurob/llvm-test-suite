! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf05 11 22 33 44"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf05
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf05.f
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
!*                             : which are invoked within parallel region
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

        DATA CmdLine/'fxcllf05 11 22 33 44'/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf05

      USE MOD




      SELECTCASE(COMMAND_ARGUMENT_COUNT())
      CASE (4)
      CASE DEFAULT
        error stop 71
      END SELECT


      SELECTCASE(INT_GET_CMD())
      CASE ('fxcllf05 11 22 33 44')
      CASE DEFAULT
        error stop 72
      END SELECT


      SELECTCASE(INT_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT()))
      CASE ('44')
      CASE DEFAULT
        error stop 73
      END SELECT


      SELECTCASE(INT_GET_ENV_VAR())
      CASE ('fxcllf05 11 22 33 44')
      CASE DEFAULT
        error stop 74
      END SELECT


     CONTAINS


      FUNCTION INT_GET_CMD()


      character(2049)  :: COMMAND, INT_GET_CMD
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT


      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j


      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
          error stop 64
      endif

      INT_GET_CMD = TRIM(COMMAND)

      END FUNCTION



      FUNCTION INT_GET_CMD_ARG(CmdCount)

      USE MOD

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE, INT_GET_CMD_ARG
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j


      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

         INT_GET_CMD_ARG =  TRIM(VALUE)   ! only return the final option
      END DO

     END FUNCTION



      FUNCTION INT_GET_ENV_VAR()

      USE MOD

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE, INT_GET_ENV_VAR
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         error stop 66
      endif

      INT_GET_ENV_VAR = TRIM(VALUE)

      END FUNCTION

      END

      INCLUDE 'cmdline.include'



