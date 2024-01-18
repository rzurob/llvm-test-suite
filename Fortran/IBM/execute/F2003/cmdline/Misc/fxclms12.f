! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclms12 . . . . ."
! %COMPOPTS:  -qfixed=132
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclms12
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclms12.f
!*  TEST CASE TITLE            : Command Line Intrinsic Procedures
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct 1, 2003
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
!*  DESCRIPTION                : Use procedure name as part of array name 
!*                             : like "call cammand_argument_count(1) = cammand_argument_count()"
!*                             : Test the fixed form
!*   
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

        character(300)              :: CmdLine = 'fxclms12 . . . . .'
        integer                     :: CmdCount = 5
        integer                     :: i, j
        character(300)              :: Argument

        character(300)              :: COMMAND
        integer                     :: LENGTH
        integer                     :: STATUS
        integer                     :: NUMBER
        character(300)              :: VALUE
        integer                     :: ARGCOUNT
 
      END MODULE


      PROGRAM fxclms12
      USE MOD
      IMPLICIT NONE

 
      INTEGER  CALL COMMAND_ARGUMENT_COUNT 
      LOGICAL  CALL GET_COMMAND(3) 
      LOGICAL  CALL GET_COMMAND_ARGUMENT(3)
      LOGICAL  CALL GET_ENVIRONMENT_VARIABL(3) 
 
      INTERFACE
        LOGICAL FUNCTION SF_GET_CMD()
        END FUNCTION

        LOGICAL FUNCTION SF_GET_CMD_ARG(i)
          INTEGER i 
        END FUNCTION

        LOGICAL FUNCTION SF_GET_ENV_VAR()
        END FUNCTION

      END INTERFACE

 
      CALL COMMAND_ARGUMENT_COUNT = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. CALL COMMAND_ARGUMENT_COUNT )  
     Cthen
        error stop 63
      endif

      CALL GET_COMMAND =  SF_GET_CMD()

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or.  
     C     (LENGTH .ne. LEN(TRIM(CmdLine)))    .or.  
     C     (STATUS .ne. 0) )                         
     Cthen
        error stop 74
      endif


      CALL GET_COMMAND_ARGUMENT = SF_GET_CMD_ARG(CALL COMMAND_ARGUMENT_COUNT)

      DO i  = 0, CALL COMMAND_ARGUMENT_COUNT 
       
        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or.  
     C       (LENGTH      .ne. LEN(TRIM(Argument)))  .or.  
     C       (STATUS      .ne. 0) )                        
     C  then
          error stop 65
        endif

      END DO


      CALL GET_ENVIRONMENT_VARIABL = SF_GET_ENV_VAR()

      call GET_ENVIRONMENT_VARIABLE('CmdLine', VALUE, LENGTH, STATUS, .false.)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))   .or.  
     C      (LENGTH .ne. LEN(TRIM(CmdLine)))  .or.  
     C      (STATUS .ne. 0))                        
     Cthen
         error stop 66
      endif


      END 


      FUNCTION SF_GET_CMD()
      USE MOD
      IMPLICIT NONE

      LOGICAL SF_GET_CMD

      SF_GET_CMD = .true.

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or.  
     C     (LENGTH .ne. LEN(TRIM(CmdLine)))    .or.  
     C     (STATUS .ne. 0) )                         
     Cthen
        SF_GET_CMD = .false. 
        error stop 64
      endif

      END FUNCTION 

  
      FUNCTION SF_GET_CMD_ARG(Count)
      USE MOD
      IMPLICIT NONE

      LOGICAL SF_GET_CMD_ARG
      INTEGER Count

      SF_GET_CMD_ARG = .true.

      DO i  = 0, Count
       
        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or.  
     C       (LENGTH      .ne. LEN(TRIM(Argument)))  .or.  
     C       (STATUS      .ne. 0) )                        
     C  then
          SF_GET_CMD_ARG = .false.
          error stop 65
        endif

      END DO

      END FUNCTION



      FUNCTION SF_GET_ENV_VAR() 
      USE MOD
      IMPLICIT NONE

      LOGICAL SF_GET_ENV_VAR

 
      SF_GET_ENV_VAR = .true.
      call GET_ENVIRONMENT_VARIABLE('CmdLine', VALUE, LENGTH, STATUS, .false.)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))   .or.  
     C      (LENGTH .ne. LEN(TRIM(CmdLine)))  .or.  
     C      (STATUS .ne. 0))                        
     Cthen
         SF_GET_ENV_VAR = .false.
         error stop 66
      endif


      END FUNCTION


 
      INCLUDE 'cmdline.include'

