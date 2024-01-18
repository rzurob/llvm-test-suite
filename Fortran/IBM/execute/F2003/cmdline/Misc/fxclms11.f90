! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclms11 0123456789 0123456789"
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclms11
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclms11.f
!*  TEST CASE TITLE            : Command Line Intrinsic Procedures
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov 1, 2003
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
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
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test command line intrinsic routines by passing character arguments 
!*                             : with the length less than or equal to what is needed.
!*                             :
!234567890123456789012345678901234567890123456789012345678901234567890




      PROGRAM fxclms11

      IMPLICIT NONE


      character(30)              :: CmdLine = 'fxclms11 0123456789 0123456789'
      integer                    :: CmdCount = 2
      integer                    :: i
      character(10)              :: Argument


      character(30)              :: COMMAND
      integer                    :: LENGTH
      integer                    :: STATUS
      integer                    :: NUMBER
      character(10)              :: VALUE
      integer                    :: ARGCOUNT

      

      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) & 
      then
        error stop 63
      endif

     ! Just enough to hold the content 
      call GET_COMMAND(STATUS=STATUS, COMMAND=COMMAND, LENGTH=LENGTH)
      call GET_COMMAND(STATUS=STATUS, LENGTH=LENGTH)   ! no effect on COMMAND


      if ( (COMMAND .ne. CmdLine)  .or. &
           (LENGTH .ne. 30 )       .or. &
           (STATUS .ne. 0) )            &
      then
        error stop 64
      endif

      ! Not enough to hold the content 
      call GET_COMMAND( LENGTH=LENGTH, COMMAND=COMMAND(1:29), STATUS=STATUS) 

      if ( (COMMAND(1:29) .ne. CmdLine(1:29))  .or. &
           (LENGTH .ne. 30)                    .or. &
           (STATUS .ne. -1) )                       &
      then
        error stop 65
      endif


      call GET_COMMAND(COMMAND=COMMAND)
      if ( COMMAND .ne. CmdLine)  &
      then
        error stop 66
      endif


      DO i  = 1, CmdCount
       
        NUMBER = i
        call MyGetArg(CmdLine, NUMBER, Argument)

        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call GET_COMMAND_ARGUMENT(NUMBER)  ! No effect on thers

        if ( (VALUE    .ne.  Argument)    .or. &
             (LENGTH   .ne.  10 )         .or. &
             (STATUS   .ne.  0) )              &
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

        call GET_COMMAND_ARGUMENT(VALUE =VALUE, STATUS=STATUS, NUMBER=NUMBER)
        if ( (VALUE .ne. Argument)       .or. &
             (STATUS      .ne. 0) )           &
        then
          error stop 69
        endif

      END DO


      call GET_ENVIRONMENT_VARIABLE('CmdLine   ', COMMAND, LENGTH, STATUS, .true.)
      if ( (COMMAND  .ne. CmdLine)       .or. &
           (LENGTH .ne. 30)              .or. &
           (STATUS .ne. 0))                   &
      then

        error stop 70
      endif


      call GET_ENVIRONMENT_VARIABLE('CmdLine   ', COMMAND(1:29), LENGTH, STATUS, .true.)
      if ( (COMMAND(1:29)  .ne. CmdLine(1:29))  .or. &
           (LENGTH         .ne. 30)             .or. &
           (STATUS         .ne. -1))                 &
      then
        error stop 71
      endif


      END 
 
      INCLUDE 'cmdline.include'




  
