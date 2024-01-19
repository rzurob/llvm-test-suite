! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov 1, 2003
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
!*  DESCRIPTION                : Use INTRINSIC statement and generic interface block with
!*                             : the same name as these intrinsic routines to define
!*                             : their extensions
!234567890123456789012345678901234567890123456789012345678901234567890


      MODULE MOD
        character(2049)              :: CmdLine = 'fxclms09 ..... :::: ====='
        integer                      :: CmdCount = 3
        integer                      :: i
        character(2047)              :: Argument

        character(2049)             :: COMMAND
        integer                     :: LENGTH
        integer                     :: STATUS
        integer                     :: NUMBER
        character(2047)             :: VALUE
        integer                     :: ARGCOUNT
      END MODULE


      PROGRAM fxclms09
      USE MOD

      IMPLICIT NONE


      INTRINSIC COMMAND_ARGUMENT_COUNT
      INTRINSIC GET_COMMAND
      INTRINSIC GET_COMMAND_ARGUMENT
      INTRINSIC GET_ENVIRONMENT_VARIABLE

      INTERFACE COMMAND_ARGUMENT_COUNT
        FUNCTION My_COMMAND_ARGUMENT_COUNT(i)
          INTEGER My_COMMAND_ARGUMENT_COUNT, i
        END FUNCTION
      END INTERFACE


      INTERFACE GET_COMMAND
        SUBROUTINE My_GET_COMMAND(I)
          INTEGER I
        END SUBROUTINE
      END INTERFACE

      INTERFACE GET_COMMAND_ARGUMENT
        SUBROUTINE My_GET_COMMAND_ARGUMENT()
        END SUBROUTINE
      END INTERFACE

      INTERFACE GET_ENVIRONMENT_VARIABLE
        SUBROUTINE My_GET_ENVIRONMENT_VARIABLE()
        END SUBROUTINE
      END INTERFACE


      IF ( CmdCount .ne. COMMAND_ARGUMENT_COUNT(1) ) error stop 67

      CALL GET_COMMAND(1)

      CALL GET_COMMAND_ARGUMENT

      CALL GET_ENVIRONMENT_VARIABLE


      END



      FUNCTION My_COMMAND_ARGUMENT_COUNT(NoUse)
      USE MOD
      INTEGER My_COMMAND_ARGUMENT_COUNT, NoUse

      My_COMMAND_ARGUMENT_COUNT  = COMMAND_ARGUMENT_COUNT()

      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) &
      then
        error stop 63
      endif


      END FUNCTION


      SUBROUTINE My_GET_COMMAND(NoUse)
      USE MOD
      INTEGER NoUse

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      END SUBROUTINE

      SUBROUTINE My_GET_COMMAND_ARGUMENT()
      USE MOD


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


      SUBROUTINE My_GET_ENVIRONMENT_VARIABLE()
      USE MOD

        call GET_ENVIRONMENT_VARIABLE( 'CmdLine', VALUE, LENGTH, STATUS, .true.)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif

      END SUBROUTINE


      INCLUDE 'cmdline.include'

