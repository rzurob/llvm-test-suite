! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf13 --/// -l/abc/abc/abc / / /"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf13
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf13.f
!*  TEST CASE TITLE            : Command Line Intrinsic Procedures
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Sept 18, 2003
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
!*  DESCRIPTION                : Pass COMMAND_ARGUMENT_COUNT as argument 
!*                             : Define and initialize variables in an molude
!*   
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      character(513)   :: NAME  
      logical          :: TRIM_NAME 
      character(2049)  :: CmdLine 
          

      DATA CmdLine    /'fxcllf13 --/// -l/abc/abc/abc / / /'/
      DATA NAME       /'CmdLine   '/
      DATA TRIM_NAME  /.true./


      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE  
      integer          :: ARGCOUNT 


      DATA COMMAND    / '????? '/
      DATA LENGTH     / 1111 /
      DATA STATUS     / 1111 /
      DATA NUMBER     /2222/
      DATA VALUE      / 100* '!'/
      DATA ARGCOUNT   / 0 /



      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i
      INTEGER    :: k, l, m, n


      END MODULE



      PROGRAM fxcllf13
  
      USE MOD
                                                    
      INTRINSIC COMMAND_ARGUMENT_COUNT                                                     



     if ( ICOMMAND_ARGUMENT_COUNT(COMMAND_ARGUMENT_COUNT ) .ne. 5) & 
      then
        error stop 63
      endif

     if ( JCOMMAND_ARGUMENT_COUNT(COMMAND_ARGUMENT_COUNT ) .ne. 5) & 
      then
        error stop 64
      endif
     
     if ( KCOMMAND_ARGUMENT_COUNT(COMMAND_ARGUMENT_COUNT ) .ne. 5) & 
      then
        error stop 65
      endif
 

     CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 5 ) & 
      then
        error stop 63
      endif


      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif


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


	
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         error stop 66
      endif



      CONTAINS

        FUNCTION KCOMMAND_ARGUMENT_COUNT(FUN)
          INTEGER KCOMMAND_ARGUMENT_COUNT
          INTEGER, EXTERNAL :: FUN

          KCOMMAND_ARGUMENT_COUNT =   JCOMMAND_ARGUMENT_COUNT(FUN)

        END FUNCTION

      END 
 
      INCLUDE 'cmdline.include'


      FUNCTION ICOMMAND_ARGUMENT_COUNT(FUN)
        INTEGER ICOMMAND_ARGUMENT_COUNT
        INTEGER FUN

        ICOMMAND_ARGUMENT_COUNT =  FUN()

      END FUNCTION

      FUNCTION JCOMMAND_ARGUMENT_COUNT(FUN)
        INTEGER JCOMMAND_ARGUMENT_COUNT
        INTEGER, EXTERNAL :: FUN

        JCOMMAND_ARGUMENT_COUNT =   ICOMMAND_ARGUMENT_COUNT(FUN)

      END FUNCTION



               
