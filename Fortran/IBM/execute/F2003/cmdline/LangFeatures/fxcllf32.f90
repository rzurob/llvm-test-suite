! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf32 1 a 2"
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf32
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf32.f
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
!*  DESCRIPTION                : Call command line intrinsic routine within loop-where constructs
!*                             : and with various optional arguments and intrinsic name as do construct name
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

        DATA CmdLine /"fxcllf32 1 a 2"/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf32

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
      INTEGER  Junk(10), i


      NumOfExec = .true.

      DO i = 1, 3
        WHERE (NumOfExec .eqv. .true.)
          Junk = COMMAND_ARGUMENT_COUNT()
        END WHERE

        if ( ANY(Junk .ne. 3 ) ) & 
        then
          error stop 73
        endif
      END DO

      GET_COMMAND: DO i = 1, 3
        WHERE (NumOfExec .eqv. .true.)
          LJunk = GET_CMD() 
        END WHERE 

        if ( ANY(LJunk .eqv. .false. ) ) & 
        then
          error stop 74
        endif
      END DO GET_COMMAND

      GET_COMMAND_ARGUMENT: DO i = 1, 3
      WHERE (NumOfExec .eqv. .true.)
        LJunk = GET_CMD_ARG()
      END WHERE

      if ( ANY(LJunk .eqv. .false. ) ) & 
      then
        error stop 75
      endif
      END DO GET_COMMAND_ARGUMENT

      GET_ENVIRONMENT_VARIABLE: DO i = 1, 3
      WHERE (NumOfExec .eqv. .true.)
        LJunk = GET_ENV_VAR() 
      END WHERE 

      if ( ANY(LJunk .eqv. .false. ) ) & 
      then
        error stop 76
      endif
      END DO GET_ENVIRONMENT_VARIABLE


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
        RETURN
        ! error stop 64
      endif

      call GET_COMMAND(COMMAND(33:532), LENGTH, STATUS)
      call GET_COMMAND()

      if ( (TRIM(COMMAND(33:532)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))            .or. &
           (STATUS .ne. 0) )                                &
      then
        SF_GET_CMD = .false.
        RETURN
        !call zzrcy4(64)
      endif

      call GET_COMMAND( LENGTH=LENGTH, COMMAND=COMMAND(1:101))
      if ( (TRIM(COMMAND(1:101)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine))))               &
      then
        SF_GET_CMD = .false.
        RETURN
        !call zzrcy4(64)
      endif

      call GET_COMMAND(COMMAND=COMMAND(1001:2049))
      if ( TRIM(COMMAND(1001:2049)) .ne. TRIM(CmdLine))  &
      then
        SF_GET_CMD = .false.
        RETURN
        !call zzrcy4(64)
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
          RETURN
         ! error stop 65
        endif

        call GET_COMMAND_ARGUMENT(i+NUMBER-i, VALUE(1023:2046), LENGTH, STATUS)
        call GET_COMMAND_ARGUMENT(NUMBER)

        if ( (TRIM(VALUE(1023:2046)) .ne. TRIM(Argument))   .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))         .or. &
             (STATUS      .ne. 0) )                              &
        then
          SF_GET_CMD_ARG = .false.
          RETURN
         ! error stop 65
        endif

        call GET_COMMAND_ARGUMENT(2*i + i +NUMBER-3*i, VALUE(513:688), LENGTH)
        if ( (TRIM(VALUE(513:688)) .ne. TRIM(Argument)) .or. &
             (LENGTH      .ne. LEN(TRIM(Argument))))         &
        then
          SF_GET_CMD_ARG = .false.
          RETURN
         ! error stop 65
        endif

        call GET_COMMAND_ARGUMENT(NUMBER + 0, VALUE =VALUE(11:511), STATUS=STATUS)
        if ( (TRIM(VALUE(11:511)) .ne. TRIM(Argument))       .or. &
             (STATUS      .ne. 0) )                               &
        then
          SF_GET_CMD_ARG = .false.
          RETURN
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
       call GET_ENVIRONMENT_VARIABLE('CmdLine   '// '     ' // '       ' // '      ', VALUE(1013:2039), LENGTH, STATUS, .true. .or. .true.)
      if ( (TRIM(VALUE(1013:2039)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))             .or. &
           (STATUS .ne. 0))                                  &
      then
        SF_GET_ENV_VAR  = .false.
        RETURN
        !error stop 70
      endif

      call GET_ENVIRONMENT_VARIABLE('CmdLine' // ' ' // ' ', LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.true. .and. .true.)
      if ( (LENGTH .ne.LENGTH)  .or. &
           (STATUS .ne. STATUS))     &
      then
        SF_GET_ENV_VAR  = .false.
        RETURN
        !error stop 70
      endif

      END FUNCTION
 
      INCLUDE 'cmdline.include'

