! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu19 1 a 2"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu19
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu19.f
!*  TEST CASE TITLE            : Command Line Intrinsic Procedures
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct. 1, 2003
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
!*  DESCRIPTION                : Define actual argument variables in different modules
!*                             : Invoke command line procedures through different module 
!*                             : procedures
!*                             :
!*                             : 
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD0
      character(2049)  :: COMMAND
      integer          :: LENGTH
      character(2049)  :: CmdLine 
      integer          :: CmdCOunt

      CONTAINS
      
      FUNCTION M_COMMAND_ARGUMENT_COUNT()

      INTEGER M_COMMAND_ARGUMENT_COUNT

      M_COMMAND_ARGUMENT_COUNT = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) & 
      then
        error stop 63
      endif

      END FUNCTION

      END MODULE



      MODULE MOD1

      USE MOD0

      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE

      CONTAINS

      SUBROUTINE M_GET_COMMAND

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      END SUBROUTINE


      END MODULE



      MODULE MOD2
      
      USE MOD0
      USE MOD1

      character(513)     :: NAME
      logical            :: TRIM_NAME


      CONTAINS


      SUBROUTINE M_GET_COMMAND_ARGUMENT

      character(2047)  :: Argument
 
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


      END MODULE


      MODULE MOD3

      USE MOD0
      USE MOD1
      USE MOD2

     
      CONTAINS

      SUBROUTINE M_GET_ENVIRONMENT_VARIABLE

      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      END SUBROUTINE


      END MODULE





      PROGRAM fxclpu19

      USE MOD0
      USE MOD1
      USE MOD2
      USE MOD3


      IMPLICIT NONE

      INTEGER Junk

      CmdLine   = 'fxclpu19 1 a 2'
      NAME      = 'CmdLine     '
      TRIM_NAME = .true.
      CmdCount  = 3


      Junk = M_COMMAND_ARGUMENT_COUNT()

      CALL M_GET_COMMAND

      CALL M_GET_COMMAND_ARGUMENT

      CALL M_GET_ENVIRONMENT_VARIABLE



      END



 
      INCLUDE 'cmdline.include'
