! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf30 1 a 2"
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf30
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf30.f
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
!*  DESCRIPTION                : Use command line intrinsic routine names as as where construct name
!*                             : and invoke functions which call these intrinsic routines
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

        DATA CmdLine /"fxcllf30 1 a 2"/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf30

      USE MOD
      IMPLICIT NONE


      INTERFACE 

        LOGICAL FUNCTION SF_GET_CMD()
        END FUNCTION

        LOGICAL FUNCTION SF_GET_CMD_ARG(iCount)
          INTEGER iCOUNT
        END FUNCTION

        LOGICAL FUNCTION SF_GET_ENV_VAR()
        END FUNCTION

      END INTERFACE

 
      INTEGER  CMD_ARG_COUNT
      LOGICAL  GET_CMD
      LOGICAL  GET_CMD_ARG 
      LOGICAL  GET_ENV_VAR
 

      GET_CMD()       =  SF_GET_CMD()
      GET_CMD_ARG()   =  SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT())
      GET_ENV_VAR()   =  SF_GET_ENV_VAR() 


      LOGICAL  NumOfExec(10), LJunk(10)
      INTEGER  Junk(10)


      NumOfExec = .true.


      WHERE (NumOfExec .eqv. .true.)
        Junk = COMMAND_ARGUMENT_COUNT()
      END WHERE


      if ( ANY(Junk .ne. 3 ) ) & 
      then
        error stop 73
      endif


      GET_COMMAND: WHERE (NumOfExec .eqv. .true.)
        LJunk = GET_CMD() 
      END WHERE GET_COMMAND

      if ( ANY(LJunk .eqv. .false. ) ) & 
      then
        error stop 74
      endif


      GET_COMMAND_ARGUMENT: WHERE (NumOfExec .eqv. .true.)
        LJunk = GET_CMD_ARG()
      END WHERE GET_COMMAND_ARGUMENT

      if ( ANY(LJunk .eqv. .false. ) ) & 
      then
        error stop 75
      endif


      GET_ENVIRONMENT_VARIABLE: WHERE (NumOfExec .eqv. .true.)
        LJunk = GET_ENV_VAR() 
      END WHERE GET_ENVIRONMENT_VARIABLE

      if ( ANY(LJunk .eqv. .false. ) ) & 
      then
        error stop 76
      endif



      END 


      FUNCTION SF_GET_CMD()

      USE MOD

      LOGICAL SF_GET_CMD

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      SF_GET_CMD = .true.

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        SF_GET_CMD = .false. 
        ! error stop 64
      endif

      END FUNCTION 

      FUNCTION SF_GET_CMD_ARG(CmdCount)

      USE MOD

      LOGICAL SF_GET_CMD_ARG

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          
      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      SF_GET_CMD_ARG = .true.

      DO i  = 0, CmdCount
       
        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          SF_GET_CMD_ARG = .false.
         ! error stop 65
        endif

      END DO

     END FUNCTION



      FUNCTION SF_GET_ENV_VAR() 

      USE MOD

      LOGICAL SF_GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          
      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 
      SF_GET_ENV_VAR = .true.
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         SF_GET_ENV_VAR = .false.
         ! error stop 66
      endif


      END FUNCTION
 
      INCLUDE 'cmdline.include'

