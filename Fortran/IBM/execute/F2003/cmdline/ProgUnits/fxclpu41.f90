! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu41 \^ AAAAA \^"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu41
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu41.f
!*
!*  DATE                       : Oct. 1, 2003
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
!*  DESCRIPTION                : Invoke command line procedures within external recursive functions
!*                             : through interface and return the derived type of result by
!*                             : result variables
!*                             : (Check if the intrinsic affect other storage)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      MODULE MOD

      TYPE CMD

        character        :: char0   /'?'/
        character(50)    :: COMMAND /'??????????????????????'/
        character        :: char1   /'?'/
        integer          :: LENGTH  /123/
        character        :: char2   /'?'/
        character(50)    :: CmdLine /'fxclpu41 \\^ AAAAA \\^'/
        character        :: char3   /'?'/
        integer          :: CmdCount /3/
        character        :: char4   /'?'/

        integer          :: STATUS  /321/
        character        :: char5   /'?'/
        integer          :: NUMBER  /111/
        character        :: char6   /'?'/
        character(50)    :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/
        character        :: char7   /'?'/

        character(50)    :: NAME    /'CmdLine  '/
        character        :: char8   /'?'/
        logical          :: TRIM_NAME /.true./
        character        :: char9   /'?'/

        character(50)    :: Argument

      END TYPE


      END MODULE


      PROGRAM fxclpu41
      USE MOD

      INTEGER i

      INTERFACE
        RECURSIVE FUNCTION F_GET_COMMAND(Num, Result)
        USE MOD
          TYPE(CMD) F_GET_COMMAND
          TYPE(CMD) Result
          INTEGER   Num
        END FUNCTION

        RECURSIVE FUNCTION F_GET_COMMAND_ARGUMENT(Num, Result)
        USE MOD
          TYPE(CMD) F_GET_COMMAND_ARGUMENT
          TYPE(CMD) Result
          INTEGER   Num
        END FUNCTION

        RECURSIVE FUNCTION F_GET_ENVIRONMENT_VARIABLE(Num, Result)
        USE MOD
          TYPE(CMD) F_GET_ENVIRONMENT_VARIABLE
          TYPE(CMD) Result
          INTEGER   Num
        END FUNCTION
      END INTERFACE


      INTERFACE
        FUNCTION F_COMMAND_ARGUMENT_COUNT(Num, CmdCount)
          INTEGER CmdCount
          INTEGER F_COMMAND_ARGUMENT_COUNT
          INTEGER   Num
        END FUNCTION
      END INTERFACE

      TYPE(CMD) :: Result

      IF ( F_COMMAND_ARGUMENT_COUNT(5, Result.CmdCount) .ne. Result.CmdCount) ERROR STOP 63

      Result = F_GET_COMMAND(5, Result)

      CALL Check(Result)

      Result = F_GET_COMMAND_ARGUMENT(5, Result)

      CALL Check(Result)

      Result = F_GET_ENVIRONMENT_VARIABLE(5, Result)

      CALL Check(Result)


      CONTAINS

      SUBROUTINE Check(Result)

      TYPE(CMD) Result

      IF (Result%NAME .ne. 'CmdLine  ' )              ERROR STOP 67
      IF (.not.Result%TRIM_NAME )                     ERROR STOP 68
      IF (Result%CmdCount .ne. 3)                     ERROR STOP 69
      IF (Result%CmdLine .ne. 'fxclpu41 \\^ AAAAA \\^')  ERROR STOP 70

      IF (Result%char0 .ne. '?' )                     ERROR STOP 80
      IF (Result%char1 .ne. '?' )                     ERROR STOP 81
      IF (Result%char2 .ne. '?' )                     ERROR STOP 82
      IF (Result%char3 .ne. '?' )                     ERROR STOP 83
      IF (Result%char4 .ne. '?' )                     ERROR STOP 84
      IF (Result%char5 .ne. '?' )                     ERROR STOP 85
      IF (Result%char6 .ne. '?' )                     ERROR STOP 86
      IF (Result%char7 .ne. '?' )                     ERROR STOP 87
      IF (Result%char8 .ne. '?' )                     ERROR STOP 88
      IF (Result%char9 .ne. '?' )                     ERROR STOP 89


      END SUBROUTINE

      END


      RECURSIVE FUNCTION F_COMMAND_ARGUMENT_COUNT(Num, CmdCount)
      USE MOD
      IMPLICIT NONE

      INTEGER F_COMMAND_ARGUMENT_COUNT
      INTEGER CmdCount
      INTEGER   Num

      IF (Num .gt. 1) THEN

        F_COMMAND_ARGUMENT_COUNT = F_COMMAND_ARGUMENT_COUNT(Num - 1, CmdCount)

      ELSE

        F_COMMAND_ARGUMENT_COUNT = COMMAND_ARGUMENT_COUNT()

        if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT()) &
        then
          error stop 63
        endif

      END IF

      END FUNCTION


      RECURSIVE FUNCTION F_GET_COMMAND(Num, Result) result ( R )
      USE MOD
      IMPLICIT  NONE

      TYPE(CMD) R, Result
      INTEGER   Num

      R = Result

      IF (Num .gt. 1 ) THEN

        R = F_GET_COMMAND(Num - 1, Result)

      ELSE

        call GET_COMMAND(   &
                 R%COMMAND, &
                 R%LENGTH,  &
                 R%STATUS)

        if ( (TRIM(R%COMMAND) .ne. TRIM(R%CmdLine))  .or. &
             (R%LENGTH .ne. LEN(TRIM(R%CmdLine)))    .or. &
             (R%STATUS .ne. 0) )                          &
        then
          error stop 64
        endif

      ! F_GET_COMMAND = R

      END IF

      END FUNCTION


      RECURSIVE FUNCTION F_GET_COMMAND_ARGUMENT(Num, Result) result ( R )
      USE MOD
      IMPLICIT NONE

      TYPE(CMD)  R, Result
      INTEGER    i
      INTEGER    Num

      R = Result

      IF ( Num .gt. 1 ) THEN

      R = F_GET_COMMAND_ARGUMENT(Num - 1, Result)

      ELSE

      DO i  = 0, R%CmdCount

        R%NUMBER = i
        call GET_COMMAND_ARGUMENT(                &
                R%NUMBER,    &
                R%VALUE,     &
                R%LENGTH,    &
                R%STATUS)

        call MyGetArg(R%CmdLine, &
                    R%NUMBER,  &
                    R% Argument)
        if ( (TRIM(R%VALUE) .ne. TRIM(R%Argument))       .or. &
             (R%LENGTH       .ne. LEN(trim(R%Argument))) .or. &
             (R%STATUS       .ne. 0) )                        &
        then
          error stop 65
        endif

      END DO

      ! F_GET_COMMAND_ARGUMENT = R
      END IF

      END FUNCTION


      RECURSIVE FUNCTION F_GET_ENVIRONMENT_VARIABLE(Num, Result) result ( R )
      USE MOD
      IMPLICIT  NONE

      TYPE(CMD)  R, Result
      INTEGER    Num

      R = Result


      IF ( Num .gt. 1 ) THEN

      R = F_GET_ENVIRONMENT_VARIABLE(Num - 1, Result)

      ELSE

      call GET_ENVIRONMENT_VARIABLE(   &
            R%NAME,                    &
            R%VALUE,                   &
            R%LENGTH,                  &
            R%STATUS,                  &
            R%TRIM_NAME)

      if ( (TRIM(R%VALUE) .ne. TRIM(R%CmdLine))  .or. &
           (R%LENGTH .ne. LEN(TRIM(R%CmdLine)))  .or. &
           (R%STATUS .ne. 0))                         &
      then
        error stop 66
      endif

      ! F_GET_ENVIRONMENT_VARIABLE = R

      END IF

      END FUNCTION


      INCLUDE 'cmdline.include'

