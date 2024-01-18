! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu30 1 a 2"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu30
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu30.f
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
!*  DESCRIPTION                : Invoke command line procedures within subroutines 
!*                             : of module subroutines which are called by internal 
!*                             : subroutine of external subtoutine through use association 
!*                             : 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      MODULE MOD0
      character(2049)  :: COMMAND /'??????????????????????'/
      integer          :: LENGTH  /123/
      character(2049)  :: CmdLine /'fxclpu30 1 a 2'/
      integer          :: CmdCOunt /3/

      integer          :: STATUS  /321/
      integer          :: NUMBER  /111/
      character(2047)  :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/


      character(513)   :: NAME      /'CmdLine  '/
      logical          :: TRIM_NAME /.true./

      character(2047)  :: Argument

      END MODULE

 

      MODULE MOD1
 

      CONTAINS
      
      FUNCTION M_COMMAND_ARGUMENT_COUNT()

      INTEGER M_COMMAND_ARGUMENT_COUNT

      M_COMMAND_ARGUMENT_COUNT = INT_M_COMMAND_ARGUMENT_COUNT()

      CONTAINS
      
      FUNCTION INT_M_COMMAND_ARGUMENT_COUNT()
      USE MOD0
      INTEGER INT_M_COMMAND_ARGUMENT_COUNT

      INT_M_COMMAND_ARGUMENT_COUNT = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) & 
      then
        error stop 63
      endif

      END FUNCTION

      END FUNCTION

      END MODULE



      MODULE MOD2


      CONTAINS

      SUBROUTINE M_GET_COMMAND
 
      CALL INT_M_GET_COMMAND

      CONTAINS

      SUBROUTINE INT_M_GET_COMMAND
      USE MOD0


      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      END SUBROUTINE


      END SUBROUTINE



      END MODULE



      MODULE MOD3
      

      CONTAINS


      SUBROUTINE M_GET_COMMAND_ARGUMENT

      CALL INT_M_GET_COMMAND_ARGUMENT


      CONTAINS

      SUBROUTINE INT_M_GET_COMMAND_ARGUMENT
      USE MOD0


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

      END SUBROUTINE


      END MODULE


      MODULE MOD4

     
      CONTAINS

      SUBROUTINE M_GET_ENVIRONMENT_VARIABLE

      CALL INT_M_GET_ENVIRONMENT_VARIABLE

      CONTAINS
      
      SUBROUTINE INT_M_GET_ENVIRONMENT_VARIABLE
      USE MOD0


      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      END SUBROUTINE

      END SUBROUTINE

      END MODULE





      PROGRAM fxclpu30

      INTERFACE 
        SUBROUTINE ENTRY
        END SUBROUTINE
      END INTERFACE
  
      CALL ENTRY


      END


      
      SUBROUTINE ENTRY()

      CALL INT_ENTRY

      CONTAINS
      
      SUBROUTINE INT_ENTRY()

      USE MOD1
      USE MOD2
      USE MOD3
      USE MOD4

      IMPLICIT NONE

      INTEGER Junk, i


      Junk = M_COMMAND_ARGUMENT_COUNT()

      CALL M_GET_COMMAND

      CALL M_GET_COMMAND_ARGUMENT

      CALL M_GET_ENVIRONMENT_VARIABLE
  

      END SUBROUTINE

      END SUBROUTINE


 
      INCLUDE 'cmdline.include'


  
