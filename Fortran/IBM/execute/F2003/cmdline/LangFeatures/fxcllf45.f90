! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf45 /., -/., /.,-"
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf45
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf45.f
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
!*  DESCRIPTION                : Invoke command line intrinsic routines by calling to  internal recursive 
!*                             : subprogram with actual arguments from common block
!*                             :   
!*                 
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

        character(513)   :: NAME  
        logical          :: TRIM_NAME 
        character(2049)  :: CmdLine 
          
        COMMON /sargs/CmdLine, NAME, TRIM_NAME

        character(2049)  :: COMMAND
        integer          :: LENGTH     
        integer          :: STATUS  
        integer          :: NUMBER 
        character(2047)  :: VALUE 
        integer          :: ARGCOUNT 

        COMMON /args/COMMAND, LENGTH, STATUS, NUMBER, VALUE, ARGCOUNT

      END MODULE 


      BLOCK DATA 

        character(513)   :: NAME  
        logical          :: TRIM_NAME 
        character(2049)  :: CmdLine 
          
        COMMON /sargs/CmdLine, NAME, TRIM_NAME

        DATA CmdLine/'fxcllf45 /., -/., /.,-'/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf45

      USE MOD
      IMPLICIT NONE



       IF (COMMAND_ARGUMENT_COUNT() .ne. 3 ) error stop 73

  
       IF (TRIM(F_GET_CMD()) .ne. 'fxcllf45 /., -/., /.,-' )      error stop 74


       IF (F_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT()) .ne. 0 )    error stop 75


       IF (TRIM(F_GET_ENV_VAR()) .ne. 'fxcllf45 /., -/., /.,-' )    error stop 76



      CONTAINS


      RECURSIVE FUNCTION F_GET_CMD()

      character(2049) F_GET_CMD
      INTEGER, SAVE ::  Num /3/

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
       F_GET_CMD = ' '

      IF( Num .ne. 1) THEN
          Num = Num - 1
          F_GET_CMD = F_GET_CMD() 
      ELSE

      call GET_COMMAND(F_GET_CMD, LENGTH, STATUS)
      if ( (TRIM( F_GET_CMD) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))       .or. &
           (STATUS .ne. 0) )                           &
      then 
         error stop 64
      endif

      END IF

      END FUNCTION 


      RECURSIVE FUNCTION F_GET_CMD_ARG(CmdCount) 


      INTEGER  F_GET_CMD_ARG
      INTEGER, SAVE ::  Num /3/

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      F_GET_CMD_ARG = 0

      IF( Num .ne. 1) THEN
          Num = Num - 1
          F_GET_CMD_ARG = F_GET_CMD_ARG(CmdCount)
      ELSE

      DO i  = 0, CmdCount
       
        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, F_GET_CMD_ARG )
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE)    .ne. TRIM(Argument))       .or. &
             (LENGTH         .ne. LEN(TRIM(Argument)))  .or. &
             (F_GET_CMD_ARG  .gt. 0) )                       &
        then
          error stop 65
        endif

      END DO

      END IF

      END FUNCTION



      RECURSIVE FUNCTION F_GET_ENV_VAR() 

      USE MOD
      INTEGER, SAVE        ::  Num /3/
      character(2047)      :: F_GET_ENV_VAR
          
      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      F_GET_ENV_VAR  = '????!'

      IF( Num .ne. 1) THEN
        Num = Num - 1
        F_GET_ENV_VAR = F_GET_ENV_VAR()
      ELSE

      call GET_ENVIRONMENT_VARIABLE(NAME, F_GET_ENV_VAR, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(F_GET_ENV_VAR) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        F_GET_ENV_VAR = 'WRONG ENV VARIRABLE!'  ! NoUse
        error stop 66
      endif

      END IF

      END FUNCTION


      END


 
      INCLUDE 'cmdline.include'

