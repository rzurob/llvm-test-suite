! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu12 %1 %2 %3 %-"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu12
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu12.f
!*
!*  DATE                       : Sept 18, 2003
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
!*  DESCRIPTION                : Invoke command line procedures through a module subroutine
!*                             : with actual arguments of type of pointer defined with different
!*                             : modules and assigned within main unit
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD0
      character(2049), POINTER  :: COMMAND
      integer,         POINTER  :: LENGTH
      END MODULE

      MODULE MOD1
      integer,         POINTER  :: STATUS
      integer,         POINTER  :: NUMBER
      character(2047), POINTER  :: VALUE
      END MODULE


      MODULE MOD2
      character(513)            :: NAME
      logical                   :: TRIM_NAME
      character(2049)           :: CmdLine
      END MODULE

      module MOD
      USE MOD0
      USE MOD1
      USE MOD2



      CONTAINS

      SUBROUTINE MOD_SUB(COMMAND, LENGTH, STATUS, NUMBER, VALUE)
      character(2049), POINTER  :: COMMAND
      integer,         POINTER  :: LENGTH
      integer,         POINTER  :: STATUS
      integer,         POINTER  :: NUMBER
      character(2047), POINTER  :: VALUE

      INTEGER                   :: CmdCount, i
      character(2047)           :: Argument


      CmdLine = 'fxclpu12 %1 %2 %3 %-'

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 4 ) &
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

      NAME = 'CmdLine     '
      TRIM_NAME = .true.
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (STATUS .ne. 0))                       &
      then
        error stop 66
      endif



      END SUBROUTINE



      END MODULE


      PROGRAM fxclpu12
      USE MOD
      IMPLICIT NONE

      character(2049), TARGET  :: Pte_COMMAND
      integer,         TARGET   :: Pte_LENGTH
      integer,         TARGET   :: Pte_STATUS
      integer,         TARGET   :: Pte_NUMBER
      character(2047), TARGET   :: Pte_VALUE

      COMMAND => Pte_COMMAND
      LENGTH  => Pte_LENGTH
      STATUS  => Pte_STATUS
      NUMBER  => Pte_NUMBER
      VALUE   => Pte_VALUE

      CALL MOD_SUB(COMMAND, LENGTH, STATUS, NUMBER, VALUE)

      END




      INCLUDE 'cmdline.include'

