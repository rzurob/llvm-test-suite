! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf36 1 a"
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf36
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf36.f
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
!*  DESCRIPTION                : Call command line intrinsic routines with various types of 
!*                             : RESULT variables as actual arguments
!*                             :   
!*                         
!*  
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

        character(513)   :: NAME  
        logical          :: TRIM_NAME 
        character(2049)  :: CmdLine 
          
        COMMON /sargs/CmdLine, NAME, TRIM_NAME

      END MODULE 


      BLOCK DATA 

        character(513)   :: NAME  
        logical          :: TRIM_NAME 
        character(2049)  :: CmdLine 
          
        COMMON /sargs/CmdLine, NAME, TRIM_NAME

        DATA CmdLine/'fxcllf36 1 a'/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf36

      USE MOD
      IMPLICIT NONE


      INTERFACE 

        FUNCTION F_GET_CMD(COMMAND, LENGTH, STATUS) RESULT(ResCommand)
          character(2049)  :: COMMAND
          integer          :: LENGTH     
          integer          :: STATUS  
          character(2049)  :: ResCommand
        END FUNCTION

        FUNCTION F_GET_CMD_ARG(COUNT, NUMBER, VALUE, LENGTH, STATUS) RESULT(ResStatus)
          INTEGER          :: COUNT
          integer          :: NUMBER 
          integer          :: LENGTH     
          integer          :: STATUS  
          character(2047)  :: VALUE 
          integer          :: ResStatus 
        END FUNCTION

        FUNCTION F_GET_ENV_VAR(VALUE, LENGTH, STATUS) RESULT(ResValue)
          integer          :: LENGTH     
          integer          :: STATUS  
          character(2047)  :: VALUE 
          character(2047)  :: ResValue 
        END FUNCTION

      END INTERFACE

 
      INTEGER  CMD_ARG_COUNT
      LOGICAL  GET_CMD
      LOGICAL  GET_CMD_ARG 
      LOGICAL  GET_ENV_VAR
 
      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 

      integer i



       IF (COMMAND_ARGUMENT_COUNT() .ne. 2 ) error stop 73

  
       IF (TRIM(F_GET_CMD(COMMAND, LENGTH, STATUS)) .ne. 'fxcllf36 1 a' )        error stop 74


       IF (F_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT(), NUMBER, VALUE, LENGTH, STATUS) .ne. 0 )    error stop 75


       IF (TRIM(F_GET_ENV_VAR(VALUE, LENGTH, STATUS)) .ne. 'fxcllf36 1 a' )    error stop 76



      END 


      FUNCTION F_GET_CMD(COMMAND, LENGTH, STATUS) RESULT(ResCommand)

      USE MOD

      character(2049)  ResCommand

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
     ResCommand = ' '

      call GET_COMMAND(ResCommand, LENGTH, STATUS)
      if ( (TRIM(ResCommand) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then 
         error stop 64
      endif

      END FUNCTION 

      FUNCTION F_GET_CMD_ARG(CmdCount, NUMBER, VALUE, LENGTH, STATUS) RESULT(ResStatus)

      USE MOD

      INTEGER  ResStatus

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          
      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      ResStatus = 0

      DO i  = 0, CmdCount
       
        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, ResStatus)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (ResStatus      .gt. 0) )                       &
        then
          error stop 65
        endif

      END DO

     END FUNCTION



      FUNCTION F_GET_ENV_VAR(VALUE, LENGTH, STATUS) RESULT(ResValue)

      USE MOD

      character(2047)  :: ResValue

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          
      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      ResValue = '????!'
      call GET_ENVIRONMENT_VARIABLE(NAME, ResValue, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(ResValue) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
         ResValue = 'WRONG ENV VARIRABLE!'  ! NoUse
         error stop 66
      endif


      END FUNCTION


 
      INCLUDE 'cmdline.include'


