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
!*  DESCRIPTION                : Invoke command line procedures within module subroutines
!*                             : with host entity names the same as these intrinsic names
!*                             : through use association
!*  33. Specify the host entity name the same as the procedure's generic name with use association
!234567890123456789012345678901234567890123456789012345678901234567890



      MODULE MOD0
      character(2049)  :: COMMAND /'??????????????????????'/
      integer          :: LENGTH  /123/
      character(2049)  :: CmdLine /'fxclpu33 1 a 2'/
      integer          :: CmdCOunt /3/

      integer          :: STATUS  /321/
      integer          :: NUMBER  /111/
      character(2047)  :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/


      character(513)   :: NAME      /'CmdLine  '/
      logical          :: TRIM_NAME /.true./

      character(2047)  :: Argument


      INTRINSIC COMMAND_ARGUMENT_COUNT
      INTRINSIC GET_COMMAND
      INTRINSIC GET_COMMAND_ARGUMENT
      INTRINSIC GET_ENVIRONMENT_VARIABLE


      END MODULE



      MODULE MOD1

      INTEGER COMMAND_ARGUMENT_COUNT
      INTEGER GET_COMMAND(1, 1, 1)
      INTEGER GET_COMMAND_ARGUMENT(1, 1, 1, 1)
      INTEGER GET_ENVIRONMENT_VARIABLE(1, 1, 1, 1, 1)


      CONTAINS


      SUBROUTINE MOD_SUB
      USE MOD0

      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT()) &
      then
        error stop 63
      endif

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

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

      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif


      END SUBROUTINE

      END MODULE



      PROGRAM fxclpu33

      USE MOD1

      CALL MOD_SUB


      END



      INCLUDE 'cmdline.include'



