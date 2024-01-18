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
!*  DESCRIPTION                : Test command line intrinsic routines by passing character arguments
!*                             : with the length less than or equal to what is needed and
!*                             : these character arguments are actually the same variable but from
!*                             : different sections)(even overlapped) - Share actual arguments
!234567890123456789012345678901234567890123456789012345678901234567890




      PROGRAM fxclms13

      IMPLICIT NONE


      character(30)              :: CmdLine = 'fxclms13 .......... =========='
      integer                    :: CmdCount = 2
      integer                    :: i
      character(10)              :: Argument


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

     ! Just enough to hold the content
      call GET_COMMAND(STATUS=STATUS, COMMAND=COMMAND(2:31), LENGTH=LENGTH)
      call GET_COMMAND(STATUS=STATUS, LENGTH=LENGTH)   ! no effect on COMMAND


      if ( (COMMAND(2:31) .ne. CmdLine)  .or. &
           (LENGTH .ne. 30 )       .or. &
           (STATUS .ne. 0) )            &
      then
        error stop 64
      endif

      ! Not enough to hold the content
      call GET_COMMAND( LENGTH=LENGTH, COMMAND=COMMAND(2:30), STATUS=STATUS)

      if ( (COMMAND(2:30) .ne. CmdLine(1:29))  .or. &
           (LENGTH .ne. 30)                    .or. &
           (STATUS .ne. -1) )                       &
      then
        error stop 65
      endif

      ! Share LENGTH and STATUS
      call GET_COMMAND(COMMAND(251:280), STATUS, STATUS)
      if ( COMMAND(251:280) .ne. CmdLine)  &
      then
        error stop 66
      endif


      DO i  = 1, CmdCount

        NUMBER = i
        call MyGetArg(CmdLine, NUMBER, Argument)

        call GET_COMMAND_ARGUMENT(NUMBER, VALUE(200:210), LENGTH, STATUS)
        call GET_COMMAND_ARGUMENT(NUMBER)  ! No effect on thers

        if ( (VALUE(200:210) .ne.  Argument)    .or. &
             (LENGTH         .ne.  10 )         .or. &
             (STATUS         .ne.  0) )              &
        then
          error stop 67
        endif

        call GET_COMMAND_ARGUMENT(NUMBER, VALUE(1:9), LENGTH, STATUS)
        if ( (VALUE(1:9) .ne. Argument(1:9)) .or. &
             (LENGTH     .ne. 10)            .or. &
             (STATUS     .ne. -1) )               &
        then
          error stop 68
        endif

        !Share the same var for STATUS and LENGTH
        call GET_COMMAND_ARGUMENT(VALUE =VALUE, STATUS=STATUS, NUMBER=NUMBER, LENGTH=STATUS)
        if ( (VALUE .ne. Argument) )    &
        then
          error stop 69
        endif

      END DO

      ! Share COMMAND for different arguments
      COMMAND(1:7) = 'CmdLine'
      call GET_ENVIRONMENT_VARIABLE(COMMAND(1:7), COMMAND(8:37), LENGTH, STATUS, .false.)
      if ( (COMMAND(8:37)  .ne. CmdLine)       .or. &
           (LENGTH .ne. 30)                    .or. &
           (STATUS .ne. 0))                         &
      then
        error stop 70
      endif


      call GET_ENVIRONMENT_VARIABLE(COMMAND(1:7), COMMAND(8:36), LENGTH, STATUS, .true.)
      if ( (COMMAND(8:36)  .ne. CmdLine(1:29))  .or. &
           (LENGTH         .ne. 30)             .or. &
           (STATUS         .ne. -1))                 &
      then
        error stop 71
      endif

      ! Share COMMAND for different arguments and the sections are overlapped
      COMMAND(1:7) = 'CmdLine'
      call GET_ENVIRONMENT_VARIABLE(COMMAND(1:7), COMMAND(5:34), LENGTH, STATUS, .false.)
      if ( (COMMAND(5:34)  .ne. CmdLine)       .or. &
           (LENGTH .ne. 30)                    .or. &
           (STATUS .ne. 0))                         &
      then
        error stop 72
      endif



      END

      INCLUDE 'cmdline.include'


