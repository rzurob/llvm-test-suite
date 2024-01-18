! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclms10 01234567890-01234567890=0123456789"
! %COMPOPTS:  -qfree=f90 
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclms10
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclms10.f
!*  TEST CASE TITLE            : Command Line Intrinsic Procedures
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov 1, 2003
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
!*  DESCRIPTION                : Through USE statement to rename module procedures' name 
!*                             : the same as these command line intrinsics, and call them 
!*                             : inside module routines
!*                             :
!*                             : 
!234567890123456789012345678901234567890123456789012345678901234567890


      MODULE MOD

      character(2049)              :: CmdLine = 'fxclms10 01234567890-01234567890=0123456789'
      integer                      :: CmdCount = 1
      integer                      :: i
      character(2047)              :: Argument

      character(2049)             :: COMMAND
      integer                     :: LENGTH
      integer                     :: STATUS
      integer                     :: NUMBER
      character(2047)             :: VALUE
      integer                     :: ARGCOUNT


      CONTAINS


      FUNCTION MOD_COMMAND_ARGUMENT_COUNT()
      INTEGER MOD_COMMAND_ARGUMENT_COUNT

      MOD_COMMAND_ARGUMENT_COUNT  = COMMAND_ARGUMENT_COUNT()

      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) & 
      then
        error stop 63
      endif


      END FUNCTION


      SUBROUTINE MOD_GET_COMMAND()

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      END SUBROUTINE

      SUBROUTINE MOD_GET_COMMAND_ARGUMENT()


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

      END SUBROUTINE


      SUBROUTINE MOD_GET_ENVIRONMENT_VARIABLE()

        call GET_ENVIRONMENT_VARIABLE( 'CmdLine', VALUE, LENGTH, STATUS, .true.)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif

      END SUBROUTINE

      END MODULE



      PROGRAM fxclms10

      USE MOD,  COMMAND_ARGUMENT_COUNT   => MOD_COMMAND_ARGUMENT_COUNT  
      USE MOD,  GET_COMMAND              => MOD_GET_COMMAND  
      USE MOD,  GET_COMMAND_ARGUMENT     => MOD_GET_COMMAND_ARGUMENT  
      USE MOD,  GET_ENVIRONMENT_VARIABLE => MOD_GET_ENVIRONMENT_VARIABLE  


      IMPLICIT NONE


      IF ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) error stop 67

      CALL GET_COMMAND

      CALL GET_COMMAND_ARGUMENT

      CALL GET_ENVIRONMENT_VARIABLE
 

      END 
 
 



      INCLUDE 'cmdline.include'

