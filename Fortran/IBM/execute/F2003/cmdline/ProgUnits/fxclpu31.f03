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
!*  DESCRIPTION                : Invoke command line procedures within external subroutine
!*                             : which are passed as procedure arguments into module
!*                             : subroutines
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      MODULE MOD0
      character(2049)  :: COMMAND /'??????????????????????'/
      integer          :: LENGTH  /123/
      character(2049)  :: CmdLine /'fxclpu31 ==\\! ==\\@ ++ \\~'/
      integer          :: CmdCOunt /4/

      integer          :: STATUS  /321/
      integer          :: NUMBER  /111/
      character(2047)  :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/


      character(513)   :: NAME      /'CmdLine  '/
      logical          :: TRIM_NAME /.true./

      character(2047)  :: Argument

      END MODULE



      MODULE MOD1

      INTERFACE CMDLINE
        MODULE PROCEDURE M_GET_COMMAND
        MODULE PROCEDURE M_GET_COMMAND_ARGUMENT
        MODULE PROCEDURE M_GET_ENVIRONMENT_VARIABLE
      END INTERFACE

      INTERFACE CMDLINE1
        MODULE PROCEDURE M_COMMAND_ARGUMENT_COUNT
      END INTERFACE


      CONTAINS

      FUNCTION M_COMMAND_ARGUMENT_COUNT(FUN)
      USE MOD0

      INTEGER  M_COMMAND_ARGUMENT_COUNT
      EXTERNAL FUN
      INTEGER  FUN

      M_COMMAND_ARGUMENT_COUNT = FUN()

      if ( CmdCount .ne. FUN()) &
      then
        error stop 63
      endif

      END FUNCTION


      SUBROUTINE M_GET_COMMAND(SUB)
      EXTERNAL SUB

      CALL SUB

      END SUBROUTINE


      SUBROUTINE M_GET_COMMAND_ARGUMENT(SUB, A)
      EXTERNAL SUB
      INTEGER  A

      CALL SUB

      END SUBROUTINE


      SUBROUTINE M_GET_ENVIRONMENT_VARIABLE(SUB, A, B)
      EXTERNAL SUB
      INTEGER  A
      INTEGER  B

      CALL SUB

      END SUBROUTINE

      END MODULE


      PROGRAM fxclpu31

      USE MOD1

      INTEGER Junk, i

      INTRINSIC COMMAND_ARGUMENT_COUNT
      EXTERNAL  EXT_GET_COMMAND
      EXTERNAL  EXT_GET_COMMAND_ARGUMENT
      EXTERNAL  EXT_GET_ENVIRONMENT_VARIABLE


      Junk = M_COMMAND_ARGUMENT_COUNT(COMMAND_ARGUMENT_COUNT)

      CALL M_GET_COMMAND(EXT_GET_COMMAND)

      CALL M_GET_COMMAND_ARGUMENT(EXT_GET_COMMAND_ARGUMENT, 0)

      CALL M_GET_ENVIRONMENT_VARIABLE(EXT_GET_ENVIRONMENT_VARIABLE, 0, 0)


      END


      SUBROUTINE EXT_GET_COMMAND
      USE MOD0


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

      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      END SUBROUTINE


      INCLUDE 'cmdline.include'


