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
!*  DESCRIPTION                : Invoke command line procedures within recursive subroutines
!*                             : through (entry -> entry/ Func -> entry -> entry)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD0

      character(2049)  :: COMMAND /'??????????????????????'/
      integer          :: LENGTH  /123/
      character(2049)  :: CmdLine /'fxclpu35 --\\[ --- --\\]'/
      integer          :: CmdCOunt /3/

      integer          :: STATUS  /321/
      integer          :: NUMBER  /111/
      character(2047)  :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/


      character(513)   :: NAME      /'CmdLine  '/
      logical          :: TRIM_NAME /.true./

      character(2047)  :: Argument

      END MODULE




      PROGRAM fxclpu35


      IMPLICIT NONE

      INTEGER Junk, i

      INTERFACE
        FUNCTION E_COMMAND_ARGUMENT_COUNT()
          INTEGER E_COMMAND_ARGUMENT_COUNT
        END FUNCTION
        FUNCTION ENT_E_COMMAND_ARGUMENT_COUNT()
          INTEGER E_COMMAND_ARGUMENT_COUNT
        END FUNCTION
      END INTERFACE


      DO i = 1, 5

        Junk = E_COMMAND_ARGUMENT_COUNT()
        Junk = ENT_E_COMMAND_ARGUMENT_COUNT()

        CALL E_GET_COMMAND
        CALL ENT1_E_GET_COMMAND

        CALL E_GET_COMMAND_ARGUMENT
        CALL ENT1_E_GET_COMMAND_ARGUMENT

        CALL E_GET_ENVIRONMENT_VARIABLE
        CALL ENT1_E_GET_ENVIRONMENT_VARIABLE

      END DO


      END




      RECURSIVE FUNCTION E_COMMAND_ARGUMENT_COUNT()
      USE MOD0

      INTEGER E_COMMAND_ARGUMENT_COUNT
      INTEGER ENT_E_COMMAND_ARGUMENT_COUNT
      INTEGER ENT1_E_COMMAND_ARGUMENT_COUNT


      E_COMMAND_ARGUMENT_COUNT = ENT_E_COMMAND_ARGUMENT_COUNT()
      RETURN

      ENTRY ENT_E_COMMAND_ARGUMENT_COUNT

      ENT_E_COMMAND_ARGUMENT_COUNT = ENT1_E_COMMAND_ARGUMENT_COUNT()
      RETURN

      ENTRY ENT1_E_COMMAND_ARGUMENT_COUNT


      ENT1_COMMAND_ARGUMENT_COUNT = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) &
      then
        error stop 63
      endif

      END FUNCTION


      RECURSIVE SUBROUTINE E_GET_COMMAND
      USE MOD0

      CALL ENT_E_GET_COMMAND
      RETURN


      ENTRY ENT_E_GET_COMMAND
      CALL ENT1_E_GET_COMMAND
      RETURN

      ENTRY ENT1_E_GET_COMMAND

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      END SUBROUTINE


      RECURSIVE SUBROUTINE E_GET_COMMAND_ARGUMENT
      USE MOD0

      CALL ENT_E_GET_COMMAND_ARGUMENT
      RETURN

      ENTRY ENT_E_GET_COMMAND_ARGUMENT
      CALL ENT1_E_GET_COMMAND_ARGUMENT
      RETURN

      ENTRY ENT1_E_GET_COMMAND_ARGUMENT

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



      RECURSIVE SUBROUTINE E_GET_ENVIRONMENT_VARIABLE
      USE MOD0

      CALL ENT_E_GET_ENVIRONMENT_VARIABLE
      RETURN


      ENTRY ENT_E_GET_ENVIRONMENT_VARIABLE
      CALL ENT1_E_GET_ENVIRONMENT_VARIABLE
      RETURN

      ENTRY ENT1_E_GET_ENVIRONMENT_VARIABLE

      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      END SUBROUTINE




      INCLUDE 'cmdline.include'


