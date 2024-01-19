! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept 18, 2003
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
!*  DESCRIPTION                : Call command line intrinsic routines through
!*                             : multiple levels of internal subroutines
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      character(513)   :: NAME
      logical          :: TRIM_NAME
      character(2049)  :: CmdLine


      DATA CmdLine    /'fxcllf18 1 a 2'/
      DATA NAME       /'CmdLine   '/
      DATA TRIM_NAME  /.true./


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT


      DATA COMMAND    / '????? '/
      DATA LENGTH     / 1111 /
      DATA STATUS     / 1111 /
      DATA NUMBER     /2222/
      DATA VALUE      / 1*'!'/
      DATA ARGCOUNT   / 0 /



      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i


      END MODULE



      PROGRAM fxcllf18


      CALL INT_SUB

      CONTAINS


      SUBROUTINE INT_SUB


      CALL REC_COMMAND_ARGUMENT_COUNT( COMMAND_ARGUMENT_COUNT())

      CALL REC_GET_COMMAND( COMMAND_ARGUMENT_COUNT())

      CALL REC_GET_COMMAND_ARGUMENT( COMMAND_ARGUMENT_COUNT())

      CALL REC_GET_ENVIRONMENT_VARIABLE( COMMAND_ARGUMENT_COUNT())


      END SUBROUTINE


      RECURSIVE SUBROUTINE REC_COMMAND_ARGUMENT_COUNT(Num)

      USE MOD
      INTEGER Num

      IF ( Num .le. 0 ) &
      THEN
         CmdCount = COMMAND_ARGUMENT_COUNT()
         if ( CmdCount .ne. 3 ) &
         then
           error stop 63
         endif
         RETURN
       ELSE
         CALL  REC_COMMAND_ARGUMENT_COUNT(Num - 1)
       END IF
      END SUBROUTINE



      RECURSIVE SUBROUTINE REC_GET_COMMAND(Num)

      USE MOD
      INTEGER Num

      IF ( Num .le. 0 ) &
      THEN
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif
         RETURN
       ELSE
         CALL REC_GET_COMMAND(Num - 1)
       END IF
      END SUBROUTINE


      RECURSIVE SUBROUTINE REC_GET_COMMAND_ARGUMENT(Num)

      USE MOD
      INTEGER Num

      IF ( Num .le. 0 ) &
      THEN
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
        RETURN
       ELSE
         CALL REC_GET_COMMAND_ARGUMENT(Num - 1)
       END IF


      END SUBROUTINE


      RECURSIVE SUBROUTINE REC_GET_ENVIRONMENT_VARIABLE(Num)

      USE MOD
      INTEGER Num

      IF ( Num .le. 0 ) &
      THEN
        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif
        RETURN
       ELSE
         CALL REC_GET_ENVIRONMENT_VARIABLE(Num - 1)
       END IF


      ENDSUBROUTINE

      END



      INCLUDE 'cmdline.include'
