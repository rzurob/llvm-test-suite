! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf16 ---_ _ ==_ __ // ++++"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf16
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf16.f
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
!*  DESCRIPTION                : Call command line intrinsic routines through  
!*                             : external recursive functions
!*    
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      character(513)   :: NAME  
      logical          :: TRIM_NAME 
      character(2049)  :: CmdLine 
          

      DATA CmdLine    /'fxcllf16 ---_ _ ==_ __ // ++++'/
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


      END MODULE



      PROGRAM fxcllf16
 
      INTERFACE

      RECURSIVE FUNCTION REC_COMMAND_ARGUMENT_COUNT(Num)
        LOGICAL REC_COMMAND_ARGUMENT_COUNT
        INTEGER Num
      END FUNCTION

      RECURSIVE FUNCTION REC_GET_COMMAND(Num)
        LOGICAL REC_GET_COMMAND
        INTEGER Num
      END FUNCTION

      RECURSIVE FUNCTION REC_GET_COMMAND_ARGUMENT(Num)
        LOGICAL REC_GET_COMMAND_ARGUMENT
        INTEGER Num
      END FUNCTION


      RECURSIVE FUNCTION REC_GET_ENVIRONMENT_VARIABLE(Num)
        LOGICAL REC_GET_ENVIRONMENT_VARIABLE
        INTEGER Num
      END FUNCTION


      END INTERFACE

                                                    
      IF ( REC_COMMAND_ARGUMENT_COUNT( COMMAND_ARGUMENT_COUNT()))   call zzrc( 73 )     
                                  
      IF ( REC_GET_COMMAND( COMMAND_ARGUMENT_COUNT()))              call zzrc( 74 )     

      IF ( REC_GET_COMMAND_ARGUMENT( COMMAND_ARGUMENT_COUNT()))     call zzrc( 75 )     

      IF (REC_GET_ENVIRONMENT_VARIABLE( COMMAND_ARGUMENT_COUNT()))  call zzrc( 76 )     


      END


      RECURSIVE FUNCTION REC_COMMAND_ARGUMENT_COUNT(Num)
      USE MOD
      LOGICAL REC_COMMAND_ARGUMENT_COUNT 
      INTEGER Num

      IF ( Num .le. 0 ) &
      THEN
         REC_COMMAND_ARGUMENT_COUNT = .false.
         CmdCount = COMMAND_ARGUMENT_COUNT()
         if ( CmdCount .ne. 6 ) & 
         then
           REC_COMMAND_ARGUMENT_COUNT = .true.
           error stop 63 ! Normally never returns if get here
         endif
         RETURN
       ELSE
         REC_COMMAND_ARGUMENT_COUNT = REC_COMMAND_ARGUMENT_COUNT(Num - 1)
       END IF
      END FUNCTION



      RECURSIVE FUNCTION REC_GET_COMMAND(Num)
      USE MOD
      LOGICAL REC_GET_COMMAND 
      INTEGER Num

      IF ( Num .le. 0 ) &
      THEN
        REC_GET_COMMAND = .false.
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          REC_GET_COMMAND = .true.
          error stop 64  ! Normally never returns if get here
        endif
         RETURN
       ELSE
         REC_GET_COMMAND = REC_GET_COMMAND(Num - 1)
       END IF
      END FUNCTION


      RECURSIVE FUNCTION REC_GET_COMMAND_ARGUMENT(Num)
      USE MOD
      LOGICAL REC_GET_COMMAND_ARGUMENT 
      INTEGER Num

      IF ( Num .le. 0 ) &
      THEN
        DO i  = 0, CmdCount
          NUMBER = i
          call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
          call MyGetArg(CmdLine, NUMBER, Argument)
 
          REC_GET_COMMAND_ARGUMENT = .false.
          if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
               (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
               (STATUS      .ne. 0) )                       &
          then
            REC_GET_COMMAND_ARGUMENT = .true.
            error stop 65 ! Normally never returns if get here
          endif
        END DO
        RETURN
       ELSE
         REC_GET_COMMAND_ARGUMENT = REC_GET_COMMAND_ARGUMENT(Num - 1)
       END IF


      END FUNCTION


      RECURSIVE FUNCTION REC_GET_ENVIRONMENT_VARIABLE(Num)
      USE MOD
      LOGICAL REC_GET_ENVIRONMENT_VARIABLE 
      INTEGER Num

      IF ( Num .le. 0 ) &
      THEN
        REC_GET_ENVIRONMENT_VARIABLE = .false.
        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          REC_GET_ENVIRONMENT_VARIABLE = .true.
          error stop 66 ! Normally never returns if get here
        endif
        RETURN
       ELSE
         REC_GET_ENVIRONMENT_VARIABLE = REC_GET_ENVIRONMENT_VARIABLE(Num - 1)
       END IF


      END FUNCTION




      INCLUDE 'cmdline.include'


