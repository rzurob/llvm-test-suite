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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing constants which are
!*                             : out of range with various optional arguments as intent(in) actual arguments
!*                             : Key arguments are not in normal order
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclat19

      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclat19 1 -2 --3'
      integer                      :: CmdCount, i
      character(2047)              :: Argument


      character(2049)             :: COMMAND
      integer                     :: LENGTH
      integer                     :: STATUS
      integer                     :: NUMBER
      character(2047)             :: VALUE
      integer                     :: ARGCOUNT


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
      then
        error stop 63
      endif


      call GET_COMMAND(STATUS=STATUS, COMMAND=COMMAND, LENGTH=LENGTH)
      call GET_COMMAND()
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      call GET_COMMAND(LENGTH=LENGTH, COMMAND=COMMAND)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine))))        &
      then
        error stop 65
      endif

      call GET_COMMAND(COMMAND)
      if ( TRIM(COMMAND) .ne. TRIM(CmdLine))  &
      then
        error stop 66
      endif



      NUMBER = CmdCount + 1  ! Out of Range

      call GET_COMMAND_ARGUMENT(LENGTH=LENGTH, NUMBER=NUMBER,  STATUS=STATUS, VALUE=VALUE)

      if ( STATUS .eq. 0)        &
      then
        error stop 67
      endif

      call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
      if ( STATUS .eq. 0)        &
      then
        error stop 68
      endif

      call GET_COMMAND_ARGUMENT(NUMBER, STATUS=STATUS, VALUE =VALUE)
      if ( STATUS .eq. 0)        &
      then
        error stop 69
      endif


      call GET_ENVIRONMENT_VARIABLE('_Cmd_Line_   ', VALUE, LENGTH, STATUS, .true.)
      ! no such environment variable

      if ( STATUS .eq. 0)        &
      then
        error stop 70
      endif

      call GET_ENVIRONMENT_VARIABLE('_Cmd_Line_   ', LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.false.)
      if ( STATUS .eq. 0)        &
      then
        error stop 71
      endif


      END





