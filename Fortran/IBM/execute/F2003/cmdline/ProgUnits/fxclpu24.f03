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
!*  DESCRIPTION                : Invoke command line procedures through module internal subroutine
!*                             : and entries with arguments defined in another module
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD0
      character(2049)  :: COMMAND /'??????????????????????'/
      integer          :: LENGTH  /123/
      character(2049)  :: CmdLine /'fxclpu24 \\{ \\} \\[ \\]'/
      integer          :: CmdCOunt /4/

      integer          :: STATUS  /321/
      integer          :: NUMBER  /111/
      character(2047)  :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/


      character(513)     :: NAME      /'CmdLine  '/
      logical            :: TRIM_NAME /.true./

      character(2047)  :: Argument

      END MODULE


      MODULE MOD1

      USE MOD0



      CONTAINS

      RECURSIVE FUNCTION M_COMMAND_ARGUMENT_COUNT()

      INTEGER M_COMMAND_ARGUMENT_COUNT
      INTEGER ENT_COMMAND_ARGUMENT_COUNT


      M_COMMAND_ARGUMENT_COUNT = ENT_M_COMMAND_ARGUMENT_COUNT()
      RETURN

      ENTRY ENT_M_COMMAND_ARGUMENT_COUNT

      ENT_COMMAND_ARGUMENT_COUNT = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) &
      then
        error stop 63
      endif

      END FUNCTION

      END MODULE



      MODULE MOD2

      USE MOD0



      CONTAINS

      RECURSIVE SUBROUTINE M_GET_COMMAND

      CALL ENT_M_GET_COMMAND
      RETURN


      ENTRY ENT_M_GET_COMMAND

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      END SUBROUTINE


      END MODULE



      MODULE MOD3

      USE MOD0


      CONTAINS


      RECURSIVE SUBROUTINE M_GET_COMMAND_ARGUMENT

      CALL ENT_M_GET_COMMAND_ARGUMENT
      RETURN

      ENTRY ENT_M_GET_COMMAND_ARGUMENT

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


      END MODULE


      MODULE MOD4

      USE MOD0


      CONTAINS

      RECURSIVE SUBROUTINE M_GET_ENVIRONMENT_VARIABLE

      CALL ENT_M_GET_ENVIRONMENT_VARIABLE
      RETURN


      ENTRY ENT_M_GET_ENVIRONMENT_VARIABLE


      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      END SUBROUTINE


      END MODULE





      PROGRAM fxclpu24

      USE MOD1
      USE MOD2
      USE MOD3
      USE MOD4

      IMPLICIT NONE

      INTEGER Junk, i


      DO i = 1, 10

        Junk = M_COMMAND_ARGUMENT_COUNT()

        CALL M_GET_COMMAND

        CALL M_GET_COMMAND_ARGUMENT

        CALL M_GET_ENVIRONMENT_VARIABLE

      END DO


      END




      INCLUDE 'cmdline.include'


