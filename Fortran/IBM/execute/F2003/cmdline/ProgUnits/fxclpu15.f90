! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu15 .-j-j-j-j- =j=j=j=j +I+I+I+I+I+"
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu15
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpuf15.f
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
!*  DESCRIPTION                : Declare actual arguments mixed with other data and initialize
!*                             : them within BLOCK DATA
!*                             : Invoke command line intrinsic routines through a call chain   
!*                             : from recursive internal to recursive external subs 
!*                             :   
!*  
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

        character(513)   :: NAME 
        character(513)   :: NAME1 
        logical          :: TRIM_NAME 
        character(2049)  :: CmdLine
        character(2049)  :: CmdLine1  
        integer          :: CmdCount

        COMMON /sargs/CmdLine,CmdLine1, NAME, NAME1, TRIM_NAME, CmdCount

        character(2049)  :: COMMAND
        character(2049)  :: COMMAND1
        integer          :: LENGTH     
        integer          :: STATUS  
        integer          :: NUMBER
        character(2047)  :: VALUE1  
        character(2047)  :: VALUE 
        integer          :: ARGCOUNT 

        COMMON /cargs/COMMAND, COMMAND1, LENGTH, STATUS, NUMBER, VALUE1, VALUE, ARGCOUNT


      END MODULE 


      BLOCK DATA 
        character(513)   :: NAME 
        character(513)   :: NAME1 
        logical          :: TRIM_NAME 
        character(2049)  :: CmdLine
        character(2049)  :: CmdLine1  
        integer          :: CmdCount

        COMMON /sargs/CmdLine,CmdLine1, NAME, NAME1, TRIM_NAME, CmdCount

        DATA CmdLine   /'fxclpu15 .-j-j-j-j- =j=j=j=j +I+I+I+I+I+'/
        DATA CmdLine1  /'????????????????????????'/
        DATA NAME      /'CmdLine   '/
        DATA NAME1     /'??????????'/
        DATA TRIM_NAME /.true./
        DATA CmdCount  /3/

        character(2049)  :: COMMAND
        character(2049)  :: COMMAND1
        integer          :: LENGTH     
        integer          :: STATUS  
        integer          :: NUMBER
        character(2047)  :: VALUE1  
        character(2047)  :: VALUE 
        integer          :: ARGCOUNT 

        COMMON /cargs/COMMAND, COMMAND1, LENGTH, STATUS, NUMBER, VALUE1, VALUE, ARGCOUNT

        DATA COMMAND  /'???????????????????'/
        DATA COMMAND1 /'                   '/
        DATA LENGTH   /9999/
        DATA STATUS   /9999/
        DATA NUMBER   /9999/
        DATA VALUE1    /'              '/
        DATA VALUE    /'???????????????'/
        DATA ARGCOUNT /9999/


      END BLOCK DATA



      PROGRAM fxclpu15


      USE MOD
      IMPLICIT NONE


      INTERFACE 

        RECURSIVE SUBROUTINE S_GET_CMD
        END SUBROUTINE

        RECURSIVE SUBROUTINE S_GET_CMD_ARG
        END SUBROUTINE

        RECURSIVE SUBROUTINE S_GET_ENV_VAR
        END SUBROUTINE

      END INTERFACE

      
      CALL INT_SUB


      CONTAINS

      RECURSIVE SUBROUTINE INT_SUB 

      INTEGER, SAVE :: Num /4/
      
      IF (Num .gt. 1) THEN
        
        Num =Num - 1
        CALL INT_SUB
      ELSE

        IF (COMMAND_ARGUMENT_COUNT() .ne. CmdCount ) error stop 63

  
        CALL S_GET_CMD


        CALL S_GET_CMD_ARG


        CALL S_GET_ENV_VAR

      END IF


      END SUBROUTINE


      END



      RECURSIVE SUBROUTINE S_GET_CMD()

      USE MOD

      INTEGER, SAVE :: Num /4/
      
      IF (Num .gt. 1) THEN
        
        Num =Num - 1
        CALL S_GET_CMD 
      ELSE


      NAME1    = '22222222222222222222222222222'
      CmdLine1 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      COMMAND1 = 'yyyyyyyyyyyyyyyyyyyyyy'
      VALUE1   = 'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv' 


      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then 
         error stop 64
      endif


      END IF


      END SUBROUTINE



      RECURSIVE SUBROUTINE S_GET_CMD_ARG()

      USE MOD

      INTEGER  i
      CHARACTER(2049) Argument

      INTEGER, SAVE :: Num /4/
      
      IF (Num .gt. 1) THEN
        
        Num =Num - 1
        CALL S_GET_CMD_ARG
      ELSE


      NAME1    = '22222222222222222222222222222'
      CmdLine1 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      COMMAND1 = 'yyyyyyyyyyyyyyyyyyyyyy'
      VALUE1   = 'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv' 
 

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

      END IF


      END SUBROUTINE



      RECURSIVE SUBROUTINE S_GET_ENV_VAR() 

      USE MOD

      INTEGER, SAVE :: Num /4/
      
      IF (Num .gt. 1) THEN
        
        Num =Num - 1
        CALL S_GET_ENV_VAR 
      ELSE

      NAME1    = '22222222222222222222222222222'
      CmdLine1 = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      COMMAND1 = 'yyyyyyyyyyyyyyyyyyyyyy'
      VALUE1   = 'vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv'


      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
         error stop 66
      endif


      END IF


      END SUBROUTINE



 
      INCLUDE 'cmdline.include'


