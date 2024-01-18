! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu39 1 a 2 b 3"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu39
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu39.f
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
!*  DESCRIPTION                : Invoke command line procedures within module routines
!*                             : Pass COMMAND_ARGUMENT_COUNT as optional argument through the call chain
!*                             : (module routine -> module routine)
!*                             :
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD0
      character(2049)  :: COMMAND /'??????????????????????'/
      integer          :: LENGTH  /123/
      character(2049)  :: CmdLine /'fxclpu39 1 a 2 b 3'/
      integer          :: CmdCount /5/

      integer          :: STATUS  /321/
      integer          :: NUMBER  /111/
      character(2047)  :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/


      character(513)   :: NAME      /'CmdLine  '/
      logical          :: TRIM_NAME /.true./

      character(2047)  :: Argument

      END MODULE


      MODULE MOD1

      CONTAINS

      FUNCTION M_COMMAND_ARGUMENT_COUNT(COMMAND_ARGUMENT_COUNT)
      IMPLICIT NONE

      INTEGER M_COMMAND_ARGUMENT_COUNT
      EXTERNAL          :: COMMAND_ARGUMENT_COUNT
      INTEGER, OPTIONAL ::  COMMAND_ARGUMENT_COUNT


      IF (.not.PRESENT(COMMAND_ARGUMENT_COUNT)) RETURN

      M_COMMAND_ARGUMENT_COUNT = M_COMMAND_ARGUMENT_COUNT_1(COMMAND_ARGUMENT_COUNT=COMMAND_ARGUMENT_COUNT)

      END FUNCTION


      FUNCTION M_COMMAND_ARGUMENT_COUNT_1(COMMAND_ARGUMENT_COUNT)
      USE MOD0
      IMPLICIT NONE

      INTEGER M_COMMAND_ARGUMENT_COUNT_1
      EXTERNAL          :: COMMAND_ARGUMENT_COUNT
      INTEGER, OPTIONAL ::  COMMAND_ARGUMENT_COUNT

      IF (.not.PRESENT(COMMAND_ARGUMENT_COUNT)) RETURN

      M_COMMAND_ARGUMENT_COUNT_1 = COMMAND_ARGUMENT_COUNT()

      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT()) &
      then
        error stop 63
      endif

      END FUNCTION


      SUBROUTINE M_GET_COMMAND(GET_COMMAND)
      IMPLICIT NONE

      EXTERNAL ::  GET_COMMAND
      OPTIONAL ::  GET_COMMAND


      IF (.not.PRESENT(GET_COMMAND)) RETURN

      CALL  M_GET_COMMAND_1(GET_COMMAND=GET_COMMAND)

      END SUBROUTINE


      SUBROUTINE M_GET_COMMAND_1(GET_COMMAND)
      IMPLICIT NONE

      EXTERNAL ::  GET_COMMAND
      OPTIONAL ::  GET_COMMAND

      IF (.not.PRESENT(GET_COMMAND)) RETURN

      call GET_COMMAND

      END SUBROUTINE


      SUBROUTINE M_GET_COMMAND_ARGUMENT(GET_COMMAND_ARGUMENT)
      IMPLICIT NONE

      EXTERNAL ::  GET_COMMAND_ARGUMENT
      OPTIONAL ::  GET_COMMAND_ARGUMENT


      IF (PRESENT(GET_COMMAND_ARGUMENT)) RETURN

      CALL  M_GET_COMMAND_ARGUMENT_1(GET_COMMAND_ARGUMENT=GET_COMMAND_ARGUMENT)


      END SUBROUTINE


      SUBROUTINE M_GET_COMMAND_ARGUMENT_1(GET_COMMAND_ARGUMENT)
      IMPLICIT NONE

      EXTERNAL ::  GET_COMMAND_ARGUMENT
      OPTIONAL ::  GET_COMMAND_ARGUMENT

      IF (PRESENT(GET_COMMAND_ARGUMENT)) RETURN

      CALL  M_GET_COMMAND_ARGUMENT(GET_COMMAND_ARGUMENT=GET_COMMAND_ARGUMENT)

      END SUBROUTINE


      SUBROUTINE M_GET_ENVIRONMENT_VARIABLE(GET_ENVIRONMENT_VARIABLE)
      IMPLICIT NONE

      EXTERNAL ::  GET_ENVIRONMENT_VARIABLE
      OPTIONAL ::  GET_ENVIRONMENT_VARIABLE


      IF (.not.PRESENT(GET_ENVIRONMENT_VARIABLE)) RETURN

      CALL M_GET_ENVIRONMENT_VARIABLE_1(GET_ENVIRONMENT_VARIABLE=GET_ENVIRONMENT_VARIABLE)

      END SUBROUTINE


      SUBROUTINE M_GET_ENVIRONMENT_VARIABLE_1(GET_ENVIRONMENT_VARIABLE)
      IMPLICIT NONE

      EXTERNAL ::  GET_ENVIRONMENT_VARIABLE
      OPTIONAL ::  GET_ENVIRONMENT_VARIABLE

      IF (.not.PRESENT(GET_ENVIRONMENT_VARIABLE)) RETURN

      CALL GET_ENVIRONMENT_VARIABLE

      END SUBROUTINE



      END MODULE


      PROGRAM fxclpu39
      USE MOD1

      INTEGER Junk, i

      INTRINSIC COMMAND_ARGUMENT_COUNT
      EXTERNAL  EXT_GET_COMMAND
      EXTERNAL  EXT_GET_COMMAND_ARGUMENT
      EXTERNAL  EXT_GET_ENVIRONMENT_VARIABLE


      Junk = M_COMMAND_ARGUMENT_COUNT(COMMAND_ARGUMENT_COUNT=COMMAND_ARGUMENT_COUNT)

      CALL M_GET_COMMAND(GET_COMMAND=EXT_GET_COMMAND)

      CALL M_GET_COMMAND_ARGUMENT( GET_COMMAND_ARGUMENT=EXT_GET_COMMAND_ARGUMENT)

      CALL M_GET_ENVIRONMENT_VARIABLE(GET_ENVIRONMENT_VARIABLE=EXT_GET_ENVIRONMENT_VARIABLE)


      END



      SUBROUTINE EXT_GET_COMMAND
      USE MOD0
      IMPLICIT  NONE


      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      END SUBROUTINE


      SUBROUTINE EXT_GET_COMMAND_ARGUMENT
      USE MOD0
      IMPLICIT NONE

      INTEGER i

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

      END DO

      END SUBROUTINE


      SUBROUTINE EXT_GET_ENVIRONMENT_VARIABLE
      USE MOD0
      IMPLICIT  NONE

      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      END SUBROUTINE


      INCLUDE 'cmdline.include'


