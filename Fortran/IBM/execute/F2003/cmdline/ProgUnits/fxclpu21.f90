! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu21 //\$// //\#// //%//"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu21
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu21.f
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
!*  DESCRIPTION                : Use module procedure statement to define a module
!*                             : generic interface and invoke these intrinsic procedures in
!*                             : module subroutines.
!*                             : (the generic interface name will be used in the equivalence
!*                             : statement which causes generic entities inaccessable)
!*                             : 
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD 


      character(2049)  :: COMMAND /'??????????????????????'/
      integer          :: LENGTH  /123/

      character(2049)  :: CmdLine   /'fxclpu21 //$// //\\#// //%//'/
      integer          :: CmdCOunt  /3/
      character(513)   :: NAME      /'CmdLine  '/
      logical          :: TRIM_NAME /.true./

      integer          :: STATUS  /321/
      integer          :: NUMBER  /111/
      character(2047)  :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/



      INTERFACE MOD_INTF
        MODULE PROCEDURE             &
          M_GET_COMMAND,             &
          M_GET_COMMAND_ARGUMENT,    &
          M_GET_ENVIRONMENT_VARIABLE
      END INTERFACE




      CONTAINS

      SUBROUTINE M_EQUIVALENCE


      character(2049)  :: CmdLine   
      integer          :: CmdCOunt  
      character(513)   :: NAME      
      logical          :: TRIM_NAME 


      EQUIVALENCE (CmdLine,    MOD_INTF)
      EQUIVALENCE (NAME,       MOD_INTF)
      EQUIVALENCE (NAME,       MOD_INTF)
      EQUIVALENCE (TRIM_NAME , MOD_INTF)


      CmdLine  ='XXXXXXXXXXXXXXXXXXXXXXX'
      CmdCOunt = 333
      NAME     = 'AAAAAAAAAA'
      TRIM_NAME= .false.

      ! There is no effect on globe variables

      
      END SUBROUTINE


      FUNCTION M_COMMAND_ARGUMENT_COUNT()

      INTEGER M_COMMAND_ARGUMENT_COUNT

      M_COMMAND_ARGUMENT_COUNT = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) & 
      then
        error stop 63
      endif

      END FUNCTION


      SUBROUTINE M_GET_COMMAND(A)
    
      INTEGER A

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      END SUBROUTINE


      SUBROUTINE M_GET_COMMAND_ARGUMENT(A, B)

      INTEGER A
      INTEGER B


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


      SUBROUTINE M_GET_ENVIRONMENT_VARIABLE(A, B, C)

      INTEGER A
      INTEGER B
      INTEGER C


      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      END SUBROUTINE



      END MODULE




      PROGRAM fxclpu21

      USE MOD

      IMPLICIT NONE

      INTEGER Junk, i


      DO i = 1, 10

        Junk = M_COMMAND_ARGUMENT_COUNT()

        CALL M_EQUIVALENCE

        CALL MOD_INTF(1)

        CALL MOD_INTF(1, 1)

        CALL MOD_INTF(1, 1, 1)
  
      END DO


      END



 
      INCLUDE 'cmdline.include'

