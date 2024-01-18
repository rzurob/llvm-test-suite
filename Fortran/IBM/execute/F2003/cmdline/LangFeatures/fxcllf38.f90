! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf38 /ABC/ /DEF/ -/123/"
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf38
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf38.f
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
!*  DESCRIPTION                : Invoke command line intrinsic routines by calling to recursive external  
!*                             : functions and with result variables as arguments
!*                             :   
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

        DATA CmdLine/'fxcllf38 /ABC/ /DEF/ -/123/'/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf38

      USE MOD
      IMPLICIT NONE


      INTERFACE 

        RECURSIVE FUNCTION F_GET_CMD(COMMAND, LENGTH, STATUS) RESULT(ResCommand)
          character(2049)  :: COMMAND
          integer          :: LENGTH     
          integer          :: STATUS  
          character(2049)  :: ResCommand
        END FUNCTION

        RECURSIVE FUNCTION F_GET_CMD_ARG(COUNT, NUMBER, VALUE, LENGTH, STATUS) RESULT(ResStatus)
          INTEGER          :: COUNT
          integer          :: NUMBER 
          integer          :: LENGTH     
          integer          :: STATUS  
          character(2047)  :: VALUE 
          integer          :: ResStatus 
        END FUNCTION

        RECURSIVE FUNCTION F_GET_ENV_VAR(VALUE, LENGTH, STATUS) RESULT(ResValue)
          integer          :: LENGTH     
          integer          :: STATUS  
          character(2047)  :: VALUE 
          character(2047)  :: ResValue 
        END FUNCTION

      END INTERFACE

 
 
      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 

      integer i



       IF (COMMAND_ARGUMENT_COUNT() .ne. 3 ) error stop 73

  
       IF (TRIM(F_GET_CMD(COMMAND, LENGTH, STATUS)) .ne. 'fxcllf38 /ABC/ /DEF/ -/123/' )      error stop 74


       IF (F_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT(), NUMBER, VALUE, LENGTH, STATUS) .ne. 0 )    error stop 75


       IF (TRIM(F_GET_ENV_VAR(VALUE, LENGTH, STATUS)) .ne. 'fxcllf38 /ABC/ /DEF/ -/123/' )    error stop 76



      END 


      RECURSIVE FUNCTION F_GET_CMD(COMMAND, LENGTH, STATUS) RESULT(ResCommand)

      USE MOD

      character(2049)  ResCommand
      INTEGER, SAVE ::  Num /3/

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

      IF( Num .ne. 1) THEN
          Num = Num - 1
          ResCommand = F_GET_CMD(COMMAND, LENGTH, STATUS) 
      ELSE

      call GET_COMMAND(ResCommand, LENGTH, STATUS)
      if ( (TRIM(ResCommand) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then 
         error stop 64
      endif

      END IF

      END FUNCTION 

      RECURSIVE FUNCTION F_GET_CMD_ARG(CmdCount, NUMBER, VALUE, LENGTH, STATUS) RESULT(ResStatus)

      USE MOD

      INTEGER  ResStatus
      INTEGER, SAVE ::  Num /3/

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

      IF( Num .ne. 1) THEN
          Num = Num - 1
          ResStatus = F_GET_CMD_ARG(CmdCount, NUMBER, VALUE, LENGTH, STATUS)
      ELSE

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

      END IF

      END FUNCTION



      RECURSIVE FUNCTION F_GET_ENV_VAR(VALUE, LENGTH, STATUS) RESULT(ResValue)

      USE MOD
      INTEGER, SAVE ::  Num /3/
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

      IF( Num .ne. 1) THEN
          Num = Num - 1
          ResValue = F_GET_ENV_VAR(VALUE, LENGTH, STATUS)
      ELSE

      call GET_ENVIRONMENT_VARIABLE(NAME, ResValue, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(ResValue) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
         ResValue = 'WRONG ENV VARIRABLE!'  ! NoUse
         error stop 66
      endif

      END IF

      END FUNCTION


 
      INCLUDE 'cmdline.include'


