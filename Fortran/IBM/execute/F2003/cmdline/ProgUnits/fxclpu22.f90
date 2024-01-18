! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu22 1 a 2"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu22
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu22.f
!*
!*  DATE                       : Oct. 1, 2003
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
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Invoke these intrinsic procedures through multi level of
!*                             : module internal function, subroutine, entries and interface
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD


      character(2049)  :: COMMAND /'??????????????????????'/
      integer          :: LENGTH  /123/

      character(2049)  :: CmdLine   /'fxclpu22 1 a 2'/
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

      CALL M_GET_COMMAND(1)

      CALL M_GET_COMMAND_ARGUMENT(1, 2)

      CALL M_GET_ENVIRONMENT_VARIABLE(3, 2, 1)

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
      INTEGER B
      INTEGER C

      character(2047)  :: Argument


      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif


      ENTRY M_GET_COMMAND_ARGUMENT(A, B)


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


      ENTRY M_GET_ENVIRONMENT_VARIABLE(A, B, C)


      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif

      END SUBROUTINE



      END MODULE




      PROGRAM fxclpu22

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

