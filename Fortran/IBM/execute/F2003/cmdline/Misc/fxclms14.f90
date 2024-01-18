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
!*  DESCRIPTION                : Invoke command_argument_count as actual arguments of some inline
!*                             : functions like mod/ishift etc.
!234567890123456789012345678901234567890123456789012345678901234567890




      PROGRAM fxclms14

      IMPLICIT NONE


      character(300)             :: CmdLine = 'fxclms14 111111111 2222 33 -'
      integer                    :: CmdCount = 4
      integer                    :: i
      character(100)             :: Argument


      character(300)             :: COMMAND
      integer                    :: LENGTH
      integer                    :: STATUS
      integer                    :: NUMBER
      character(300)             :: VALUE
      integer                    :: ARGCOUNT



      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) &
      then
        error stop 63
      endif

      if (MOD(COMMAND_ARGUMENT_COUNT(), COMMAND_ARGUMENT_COUNT() + 1 )  &
         .ne. COMMAND_ARGUMENT_COUNT() )                                &
      then
        error stop 64
      endif

      if (ISHFT(COMMAND_ARGUMENT_COUNT(), 0 )  &
         .ne. COMMAND_ARGUMENT_COUNT())         &
      then
        error stop 65
      endif

      if (IAND(COMMAND_ARGUMENT_COUNT(), COMMAND_ARGUMENT_COUNT() )  &
         .ne. COMMAND_ARGUMENT_COUNT() )                             &
      then
        error stop 66
      endif

      if (IOR (COMMAND_ARGUMENT_COUNT(), COMMAND_ARGUMENT_COUNT() )  &
         .ne. COMMAND_ARGUMENT_COUNT() )                             &
      then
        error stop 66
      endif

      CALL GETENV('CmdLine', VALUE)
      call GET_ENVIRONMENT_VARIABLE('CmdLine', COMMAND, LENGTH, STATUS, .false.)
      if ( (TRIM(COMMAND)  .ne. TRIM(CmdLine))   .or. &
           (TRIM(VALUE)    .ne. TRIM(CmdLine))   .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))                      .or. &
           (STATUS .ne. 0))                           &
      then
        error stop 70
      endif


      END




