! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf37 -i -j -k - -"
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf37
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf37.f
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
!*  DESCRIPTION                : Invoke command line intrinsic routines recursively 
!*                             : through array constructor 
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

        DATA CmdLine /"fxcllf37 -i -j -k - -"/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxcllf33

      USE MOD
      IMPLICIT NONE


      INTERFACE 

        RECURSIVE LOGICAL FUNCTION SF_GET_CMD()
        END FUNCTION

        RECURSIVE LOGICAL FUNCTION SF_GET_CMD_ARG(iCount)
          INTEGER iCOUNT
        END FUNCTION

        RECURSIVE LOGICAL FUNCTION SF_GET_ENV_VAR()
        END FUNCTION

      END INTERFACE

 
      INTEGER  CMD_ARG_COUNT
      LOGICAL  GET_CMD
      LOGICAL  GET_CMD_ARG 
      LOGICAL  GET_ENV_VAR
 

      LOGICAL LJunk(3)
      INTEGER NJunk(3)

      NJunk = (/COMMAND_ARGUMENT_COUNT(),  &
                COMMAND_ARGUMENT_COUNT(),  &
                COMMAND_ARGUMENT_COUNT()   /)

      if ( ANY(NJunk .ne. 5 ) ) & 
      then
        error stop 73
      endif

      LJunk = (/ SF_GET_CMD(),  &
                 SF_GET_CMD(),  &
                 SF_GET_CMD()   /)

      if ( ANY(LJunk .eqv. .false. ) ) & 
      then
        error stop 74
      endif

      LJunk = (/  SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT()),  &
                  SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT()),  &
                  SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT())   /)

      if ( ANY(LJunk .eqv. .false. ) ) & 
      then
        error stop 75
      endif

      LJunk = (/ SF_GET_ENV_VAR(),  &
                 SF_GET_ENV_VAR(),  &
                 SF_GET_ENV_VAR()   /)

      if ( ANY(LJunk .eqv. .false. ) ) & 
      then
        error stop 76
      endif


      END 


      RECURSIVE FUNCTION SF_GET_CMD()

      USE MOD

      LOGICAL SF_GET_CMD
      INTEGER, SAVE :: Num / 5/

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

      IF (Num .ne. 1) THEN
          Num = Num - 1
          SF_GET_CMD =   SF_GET_CMD()
      ELSE

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        SF_GET_CMD = .false. 
        ! error stop 64
      endif

      END IF 


      END FUNCTION 


      RECURSIVE FUNCTION SF_GET_CMD_ARG(CmdCount)

      USE MOD

      LOGICAL SF_GET_CMD_ARG
      INTEGER, SAVE :: Num / 5/

      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE 
      integer          :: ARGCOUNT 
          
      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j
 

      SF_GET_CMD_ARG  = .true.

      IF (Num .ne. 1) THEN
          Num = Num - 1
           SF_GET_CMD_ARG =  SF_GET_CMD_ARG(CmdCount)
      ELSE


      DO i  = 0, CmdCount
       
        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
           SF_GET_CMD_ARG  = .false.
         ! error stop 65
        endif

      END DO

      END IF


      END FUNCTION



      RECURSIVE FUNCTION SF_GET_ENV_VAR() 

      USE MOD

      LOGICAL SF_GET_ENV_VAR
      INTEGER, SAVE :: Num / 5/
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

      IF (Num .ne. 1) THEN
          Num = Num - 1
          SF_GET_ENV_VAR = SF_GET_ENV_VAR()
      ELSE

        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
              (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
              (STATUS .ne. 0))                       &
        then
           SF_GET_ENV_VAR = .false.
           ! error stop 66
        endif

      END IF

      END FUNCTION
 
      INCLUDE 'cmdline.include'



