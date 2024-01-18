! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf24 1"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf24
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf24.f
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
!*  DESCRIPTION                : Call command line intrinsic routines within routines  
!*                             : with  entry names the same as these intrinsic routines 
!*   
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      character(513)   :: NAME  
      logical          :: TRIM_NAME 
      character(2049)  :: CmdLine 
          

      DATA CmdLine    /'fxcllf24 1'/
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
      DATA VALUE      / 1*'!'/
      DATA ARGCOUNT   / 0 /



      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i

      CONTAINS

      SUBROUTINE M_SUB


        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 1 ) & 
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


      END SUBROUTINE


      END MODULE



      PROGRAM fxcllf24

      USE MOD

      INTERFACE 
        SUBROUTINE COMMAND_ARGUMENT_COUNT
        END SUBROUTINE

        SUBROUTINE GET_COMMAND()
        END SUBROUTINE

        SUBROUTINE GET_COMMAND_ARGUMENT()
        END SUBROUTINE

        SUBROUTINE GET_ENVIRONMENT_VARIABLE()
        END SUBROUTINE
      END INTERFACE



      CALL COMMAND_ARGUMENT_COUNT( ) 
      CALL M_SUB

      CALL GET_COMMAND()
      CALL M_SUB

      CALL GET_COMMAND_ARGUMENT( ) 
      CALL M_SUB

      CALL GET_ENVIRONMENT_VARIABLE( )
      CALL M_SUB


      END 


      SUBROUTINE S_COMMAND_ARGUMENT_COUNT()

      USE MOD
 
      ENTRY COMMAND_ARGUMENT_COUNT



      END SUBROUTINE



      SUBROUTINE S_GET_COMMAND()

      USE MOD
      ENTRY GET_COMMAND


      END SUBROUTINE


      SUBROUTINE S_GET_COMMAND_ARGUMENT()
     
      USE MOD

      ENTRY GET_COMMAND_ARGUMENT()


      END SUBROUTINE


      SUBROUTINE S_GET_ENVIRONMENT_VARIABLE()

      USE MOD

      ENTRY GET_ENVIRONMENT_VARIABLE

      ENDSUBROUTINE



      INCLUDE 'cmdline.include'

