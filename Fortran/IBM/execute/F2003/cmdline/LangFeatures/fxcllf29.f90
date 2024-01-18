! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf29 LLLLLLLLLLLLLLLLLLLLLL\&"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf29
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf29.f
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
!*  DESCRIPTION                : Call command line intrinsic routines within functions which
!*                             : are passed as actual arguments of a subroutine and invoked
!*                             : within where constructs
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

        character(513)   :: NAME
        logical          :: TRIM_NAME
        character(2049)  :: CmdLine
        integer          :: CmdCount

        COMMON /args/CmdLine, NAME, TRIM_NAME, CmdCount

      END MODULE


      BLOCK DATA

        character(513)   :: NAME
        logical          :: TRIM_NAME
        character(2049)  :: CmdLine
        integer          :: CmdCount

        COMMON /args/CmdLine, NAME, TRIM_NAME, CmdCount

        DATA CmdLine /"LLLLLLLLLLLLLLLLLLLLLL\\&"/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf29

      USE MOD
      IMPLICIT NONE



      EXTERNAL SF_GET_CMD
      EXTERNAL SF_GET_CMD_ARG
      EXTERNAL SF_GET_ENV_VAR


      CmdCount = COMMAND_ARGUMENT_COUNT()
      IF ( CmdCount .ne. 1 )  ERROR STOP 63


      CALL SUB(SF_GET_CMD, 64)

      CALL SUB(SF_GET_CMD_ARG, 65)

      CALL SUB(SF_GET_ENV_VAR, 66)




      END

      SUBROUTINE SUB(FUN, ErrNum)
      LOGICAL FUN
      EXTERNAL FUN
      INTEGER ErrNum

      LOGICAL  NumOfExec(10), LJunk(10)

      NumOfExec = .true.

      WHERE (NumOfExec .eqv. .true.)
        LJunk = FUN()
      END WHERE

      if ( ANY(LJunk ) ) &
      then
        call zzrc(ErrNum)
      endif

      END SUBROUTINE


      FUNCTION SF_GET_CMD()

      USE MOD


      LOGICAL SF_GET_CMD

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT


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



      FUNCTION SF_GET_CMD_ARG()

      USE MOD

      LOGICAL SF_GET_CMD_ARG

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

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



      FUNCTION SF_GET_ENV_VAR()

      USE MOD

      LOGICAL SF_GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

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

