! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf43 1"
! %COMPOPTS:  -qfixed=132
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf43
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf43.f
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
!*  DESCRIPTION                : Use procedure name as as part of identifiers
!*                             : like " CHARACTER call cammand_argument_count "
!*                             : in the fixed form 
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

        DATA CmdLine/'fxcllf43 1'/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf43

      USE MOD
      IMPLICIT NONE


      INTERFACE 

        CHARACTER(2049) FUNCTION SF_GET_CMD()
        END FUNCTION

        CHARACTER(2049) FUNCTION SF_GET_CMD_ARG(iCount)
          INTEGER iCOUNT
        END FUNCTION

        CHARACTER(2049) FUNCTION SF_GET_ENV_VAR()
        END FUNCTION

      END INTERFACE

 
      CHARACTER(2049)  CALL COMMAND_ARGUMENT_COUNT 
      CHARACTER(2049)  CALL GET_COMMAND 
      CHARACTER(2049)  CALL GET_COMMAND_ARGUMENT 
      CHARACTER(2049)  CALL GET_ENVIRONMENT_VARIABL 
 

      CALL COMMAND_ARGUMENT_COUNT =  ' '
      CALL GET_COMMAND      =  SF_GET_CMD()
      CALL GET_COMMAND_ARGUMENT   =  SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT())
      CALL GET_ENVIRONMENT_VARIABL   =  SF_GET_ENV_VAR() 


      IF (COMMAND_ARGUMENT_COUNT() .ne.  1 ) error stop 72

      IF (CALL COMMAND_ARGUMENT_COUNT .ne.  ' ' ) ERROR STOP 73

      IF ( CALL GET_COMMAND .ne. ' ' )        ERROR STOP 74

      IF ( CALL GET_COMMAND_ARGUMENT .ne. ' '  )    ERROR STOP 75

      IF ( CALL GET_ENVIRONMENT_VARIABL .ne. ' '  )    ERROR STOP 76



      END 


      FUNCTION SF_GET_CMD()

      USE MOD

      CHARACTER(2049)  SF_GET_CMD

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      SF_GET_CMD = ' '

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or.  
     C     (LENGTH .ne. LEN(TRIM(CmdLine)))    .or.  
     C     (STATUS .ne. 0) )                         
     Cthen
        SF_GET_CMD = 'Err'
        ! error stop 64
      endif

      END FUNCTION 

  
      FUNCTION SF_GET_CMD_ARG(CmdCount)

      USE MOD

      CHARACTER(2049)  SF_GET_CMD_ARG

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          
      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      SF_GET_CMD_ARG = ' '

      DO i  = 0, CmdCount
       
        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or.  
     C       (LENGTH      .ne. LEN(TRIM(Argument)))  .or.  
     C       (STATUS      .ne. 0) )                        
     C  then
          SF_GET_CMD_ARG = 'Err'
         ! error stop 65
        endif

      END DO

      END FUNCTION



      FUNCTION SF_GET_ENV_VAR() 

      USE MOD

      CHARACTER(2049)  SF_GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          
      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      SF_GET_ENV_VAR = ' '
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))   .or.  
     C      (LENGTH .ne. LEN(TRIM(CmdLine)))  .or.  
     C      (STATUS .ne. 0))                        
     Cthen
         SF_GET_ENV_VAR = 'Err'
         ! error stop 66
      endif


      END FUNCTION


 
      INCLUDE 'cmdline.include'


