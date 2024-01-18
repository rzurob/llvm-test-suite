! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclms07 ------- ===== ............."
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclms07
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclms07.f
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing various 
!*                             : combination of arguments with argument keywords
!*                             : 
!*
!234567890123456789012345678901234567890123456789012345678901234567890




      PROGRAM fxclms07

      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclms07 ------- ===== .............'
      integer                      :: CmdCount = 3
      integer                      :: i
      character(2047)              :: Argument


      character(2049)             :: COMMAND
      integer                     :: LENGTH
      integer                     :: STATUS
      integer                     :: NUMBER
      character(2047)             :: VALUE
      integer                     :: ARGCOUNT

      

      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) & 
      then
        error stop 63
      endif

     ! Test if there is any effect on other arguments


      call GET_COMMAND(STATUS=STATUS, COMMAND=COMMAND, LENGTH=LENGTH)
      CALL CHK_GET_COMMAND
      call GET_COMMAND(STATUS=STATUS, LENGTH=LENGTH)  
      CALL CHK_GET_COMMAND
      call GET_COMMAND( LENGTH=LENGTH, COMMAND=COMMAND(1:20)) 
      CALL CHK_GET_COMMAND
      call GET_COMMAND(STATUS=STATUS, COMMAND=COMMAND)
      CALL CHK_GET_COMMAND
      call GET_COMMAND(COMMAND=COMMAND)
      CALL CHK_GET_COMMAND


      DO i  = 0, CmdCount
       
        NUMBER = i
        call MyGetArg(CmdLine, NUMBER, Argument)

        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        CALL CHK_GET_COMMAND_ARGUMENT
        call GET_COMMAND_ARGUMENT(NUMBER) 
        CALL CHK_GET_COMMAND_ARGUMENT
        call GET_COMMAND_ARGUMENT(LENGTH=LENGTH, NUMBER=NUMBER, VALUE=VALUE) 
        CALL CHK_GET_COMMAND_ARGUMENT
        call GET_COMMAND_ARGUMENT(VALUE=VALUE, NUMBER=NUMBER) 
        CALL CHK_GET_COMMAND_ARGUMENT
        call GET_COMMAND_ARGUMENT(VALUE =VALUE(11:511), STATUS=STATUS, NUMBER=NUMBER + mod(5, 5))

      END DO


     
      call GET_ENVIRONMENT_VARIABLE('CmdLine   ', VALUE, LENGTH, STATUS, .true.)
      CALL CHK_GET_ENVIRONMENT_VARIABLE

      call GET_ENVIRONMENT_VARIABLE(LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.true., NAME='CmdLine ' )
      CALL CHK_GET_ENVIRONMENT_VARIABLE

      call GET_ENVIRONMENT_VARIABLE(TRIM_NAME=.true., NAME='CmdLine ' )
      CALL CHK_GET_ENVIRONMENT_VARIABLE

      call GET_ENVIRONMENT_VARIABLE(VALUE=VALUE, STATUS= STATUS, TRIM_NAME=.true., NAME='CmdLine ' )
      CALL CHK_GET_ENVIRONMENT_VARIABLE

      call GET_ENVIRONMENT_VARIABLE(VALUE=VALUE, TRIM_NAME=.true., NAME='CmdLine ' )
      CALL CHK_GET_ENVIRONMENT_VARIABLE



      CONTAINS

      SUBROUTINE CHK_GET_COMMAND

      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      END SUBROUTINE 

      SUBROUTINE CHK_GET_COMMAND_ARGUMENT

      if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
           (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
           (STATUS      .ne. 0) )                       &
        then
          error stop 67
        endif

      END SUBROUTINE 

      SUBROUTINE CHK_GET_ENVIRONMENT_VARIABLE

      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 70
      endif

      END SUBROUTINE 

      END 
 
      INCLUDE 'cmdline.include'


