! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu16 %, %, %1 %3"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu16
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu16.f
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
!*  DESCRIPTION                : Call command line procedures through external functions
!*                             : which are used for the defined operators
!*                             : with actual arguments defined in derived type in module
!*                             :
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      TYPE DT

      character(2049)   :: COMMAND
      integer           :: LENGTH
      integer           :: STATUS
      integer           :: NUMBER
      character(2047)   :: VALUE
      character(513)    :: NAME
      logical           :: TRIM_NAME
      integer           :: ARGCOUNT

      END TYPE DT


      character(2049)              :: CmdLine = 'fxclpu16 %, %, %1 %3'
      integer                      :: CmdCount= 4


      END MODULE


      PROGRAM fxclpu16
      USE MOD
      IMPLICIT NONE

      INTERFACE OPERATOR ( .CMDa. )
        FUNCTION EXT_FUN1( A,B)
          USE MOD
          TYPE(DT), INTENT(in)  :: A
          TYPE(DT), INTENT(in ) :: B
          TYPE(DT)              :: EXT_FUN1
        END FUNCTION
      END INTERFACE

      INTERFACE OPERATOR ( .CMDb. )
        FUNCTION EXT_FUN2( A,B)
          USE MOD
          TYPE(DT), INTENT(in)  :: A
          TYPE(DT), INTENT(in ) :: B
          TYPE(DT)              :: EXT_FUN2
        END FUNCTION
      END INTERFACE

      INTERFACE OPERATOR ( .CMDc. )
        FUNCTION EXT_FUN3( A,B)
          USE MOD
          TYPE(DT), INTENT(in)  :: A
          TYPE(DT), INTENT(in ) :: B
          TYPE(DT)              :: EXT_FUN3
        END FUNCTION
      END INTERFACE

      INTERFACE OPERATOR ( .CMDd. )
        FUNCTION EXT_FUN4( A,B)
          USE MOD
          TYPE(DT), INTENT(in)  :: A
          TYPE(DT), INTENT(in ) :: B
          TYPE(DT)              :: EXT_FUN4
        END FUNCTION
      END INTERFACE


      TYPE(DT) :: A
      TYPE(DT) :: B



      A = A .CMDa. B
      B = A .CMDb. B
      A = A .CMDc. B
      B = A .CMDd. B


      END


      FUNCTION EXT_FUN1( A, B)
      USE MOD

      TYPE(DT), INTENT(in)  :: B
      TYPE(DT), INTENT(in)  :: A
      TYPE(DT)              :: EXT_FUN1

      integer                      :: i
      character(2047)              :: Argument


      if ( CmdCount .ne. COMMAND_ARGUMENT_COUNT() ) &
      then
        error stop 63
      endif

      EXT_FUN1 = B

      END FUNCTION



      FUNCTION EXT_FUN2( AA, BB)
      USE MOD

      TYPE(DT), INTENT(in)  :: BB
      TYPE(DT), INTENT(in)  :: AA
      TYPE(DT)              :: B
      TYPE(DT)              :: A
      TYPE(DT)              :: EXT_FUN2

      integer                      :: i
      character(2047)              :: Argument

      call GET_COMMAND(B%COMMAND, B%LENGTH, B%STATUS)
      if ( (TRIM(B%COMMAND) .ne. TRIM(CmdLine))  .or. &
           (B%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (B%STATUS .ne. 0) )                        &
      then
        error stop 64
      endif


      EXT_FUN2 = B

      END FUNCTION


      FUNCTION EXT_FUN3( AA, BB)
      USE MOD

      TYPE(DT), INTENT(in)  :: BB
      TYPE(DT), INTENT(in)  :: AA
      TYPE(DT)              :: B
      TYPE(DT)              :: A
      TYPE(DT)              :: EXT_FUN3

      integer                      :: i
      character(2047)              :: Argument

      DO i  = 0, CmdCount

        B%NUMBER = i
        call GET_COMMAND_ARGUMENT(B%NUMBER, B%VALUE, B%LENGTH, B%STATUS)
        call MyGetArg(CmdLine, B%NUMBER, Argument)
        if ( (TRIM(B%VALUE) .ne. TRIM(Argument))       .or. &
             (B%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (B%STATUS       .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO


      EXT_FUN3 = B

      END FUNCTION



      FUNCTION EXT_FUN4( AA, BB)
      USE MOD

      TYPE(DT), INTENT(in)  :: BB
      TYPE(DT), INTENT(in)  :: AA
      TYPE(DT)              :: B
      TYPE(DT)              :: A
      TYPE(DT)              :: EXT_FUN4

      integer                      :: i
      character(2047)              :: Argument


      B%NAME = 'CmdLine     '
      B%TRIM_NAME = .true.
      call GET_ENVIRONMENT_VARIABLE(B%NAME, B%VALUE, B%LENGTH, B%STATUS, B%TRIM_NAME)
      if ( (TRIM(B%VALUE) .ne. TRIM(CmdLine))  .or. &
           (B%LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (B%STATUS .ne. 0))                       &
      then
        error stop 66
      endif


      EXT_FUN4 = B

      END FUNCTION


      INCLUDE 'cmdline.include'

