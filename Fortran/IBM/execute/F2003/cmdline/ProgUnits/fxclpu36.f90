! *********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                : Invoke command line procedures within external recursive
!*                             : functions  through interface (with optional argument
!*                             : keywords same as command line intrinsic names)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      MODULE MOD0
      character(2049)  :: COMMAND /'??????????????????????'/
      integer          :: LENGTH  /123/
      character(2049)  :: CmdLine /'fxclpu36 1 + 2 = 4'/
      integer          :: CmdCount /5/

      integer          :: STATUS  /321/
      integer          :: NUMBER  /111/
      character(2047)  :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/


      character(513)   :: NAME      /'CmdLine  '/
      logical          :: TRIM_NAME /.true./

      character(2047)  :: Argument

      END MODULE


      PROGRAM fxclpu36

      INTEGER Junk, i

      INTRINSIC COMMAND_ARGUMENT_COUNT
      EXTERNAL  EXT_GET_COMMAND
      EXTERNAL  EXT_GET_COMMAND_ARGUMENT
      EXTERNAL  EXT_GET_ENVIRONMENT_VARIABLE

      INTERFACE
        RECURSIVE SUBROUTINE E_GET_COMMAND(Num, GET_COMMAND)
          EXTERNAL ::  GET_COMMAND
          OPTIONAL ::  GET_COMMAND
          INTEGER  ::  Num
        END SUBROUTINE

        RECURSIVE SUBROUTINE E_GET_COMMAND_ARGUMENT(Num, GET_COMMAND_ARGUMENT)
          EXTERNAL ::  GET_COMMAND_ARGUMENT
          OPTIONAL ::  GET_COMMAND_ARGUMENT
          INTEGER  ::  Num
        END SUBROUTINE

        RECURSIVE SUBROUTINE E_GET_ENVIRONMENT_VARIABLE(Num, GET_ENVIRONMENT_VARIABLE)
          EXTERNAL ::  GET_ENVIRONMENT_VARIABLE
          OPTIONAL ::  GET_ENVIRONMENT_VARIABLE
          INTEGER  ::  Num
        END SUBROUTINE
      END INTERFACE


      INTERFACE
        RECURSIVE FUNCTION E_COMMAND_ARGUMENT_COUNT(Num, COMMAND_ARGUMENT_COUNT)
          INTEGER E_COMMAND_ARGUMENT_COUNT
          INTEGER Num
          EXTERNAL          :: COMMAND_ARGUMENT_COUNT
          INTEGER, OPTIONAL :: COMMAND_ARGUMENT_COUNT
        END FUNCTION
      END INTERFACE


      Junk = E_COMMAND_ARGUMENT_COUNT(3, COMMAND_ARGUMENT_COUNT=COMMAND_ARGUMENT_COUNT)

      CALL E_GET_COMMAND(4, GET_COMMAND=EXT_GET_COMMAND)

      CALL E_GET_COMMAND_ARGUMENT(5, GET_COMMAND_ARGUMENT=EXT_GET_COMMAND_ARGUMENT)

      CALL E_GET_ENVIRONMENT_VARIABLE(6, GET_ENVIRONMENT_VARIABLE=EXT_GET_ENVIRONMENT_VARIABLE)


      END


      RECURSIVE FUNCTION E_COMMAND_ARGUMENT_COUNT(Num, COMMAND_ARGUMENT_COUNT)
      USE MOD0
      IMPLICIT NONE

      INTEGER Num
      INTEGER E_COMMAND_ARGUMENT_COUNT
      EXTERNAL          :: COMMAND_ARGUMENT_COUNT
      INTEGER, OPTIONAL ::  COMMAND_ARGUMENT_COUNT

      IF (.not.PRESENT(COMMAND_ARGUMENT_COUNT)) RETURN

      IF (Num .gt. 1) THEN
        E_COMMAND_ARGUMENT_COUNT =            &
           E_COMMAND_ARGUMENT_COUNT(Num - 1,  &
                                    COMMAND_ARGUMENT_COUNT=COMMAND_ARGUMENT_COUNT)
      ELSE
        E_COMMAND_ARGUMENT_COUNT = COMMAND_ARGUMENT_COUNT()

        if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT()) &
        then
          error stop 63
        endif
      END IF

      END FUNCTION


      RECURSIVE SUBROUTINE E_GET_COMMAND(Num, GET_COMMAND)
      USE MOD0
      IMPLICIT NONE

      EXTERNAL ::  GET_COMMAND
      OPTIONAL ::  GET_COMMAND
      INTEGER  ::  Num

      IF (.not.PRESENT(GET_COMMAND)) RETURN

      IF (Num .gt. 1) THEN
        CALL E_GET_COMMAND(Num - 1, GET_COMMAND = GET_COMMAND)
      ELSE
        CALL GET_COMMAND
      END IF

      END SUBROUTINE


      RECURSIVE SUBROUTINE E_GET_COMMAND_ARGUMENT(Num, GET_COMMAND_ARGUMENT)
      USE MOD0
      IMPLICIT NONE

      EXTERNAL ::  GET_COMMAND_ARGUMENT
      OPTIONAL ::  GET_COMMAND_ARGUMENT
      INTEGER  ::  Num

      IF (PRESENT(GET_COMMAND_ARGUMENT)) RETURN

      IF (Num .gt. 1) THEN
        CALL E_GET_COMMAND_ARGUMENT(Num - 1,GET_COMMAND_ARGUMENT = GET_COMMAND_ARGUMENT)
      ELSE
        CALL GET_COMMAND
      END IF

      END SUBROUTINE


      RECURSIVE SUBROUTINE E_GET_ENVIRONMENT_VARIABLE(Num,GET_ENVIRONMENT_VARIABLE)
      USE MOD0
      IMPLICIT NONE

      EXTERNAL ::  GET_ENVIRONMENT_VARIABLE
      OPTIONAL ::  GET_ENVIRONMENT_VARIABLE
      INTEGER  ::  Num

      IF (.not.PRESENT(GET_ENVIRONMENT_VARIABLE)) RETURN

      IF (Num .gt. 1) THEN
        CALL E_GET_ENVIRONMENT_VARIABLE(  &
                  Num - 1,                &
                  GET_ENVIRONMENT_VARIABLE = GET_ENVIRONMENT_VARIABLE)
      ELSE
        CALL GET_ENVIRONMENT_VARIABLE
      END IF

      END SUBROUTINE


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



