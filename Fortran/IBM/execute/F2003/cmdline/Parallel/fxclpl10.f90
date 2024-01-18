! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct 1, 2003
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
!*  DESCRIPTION                : Call command line intrinsic routines through parallel region within
!*                             : an external subroutine and with actual arguments from common block
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclpl10

      IMPLICIT NONE
      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      character(2049)      :: CmdLine

      COMMON /args/CmdLine, NAME, TRIM_NAME


      CmdLine  = 'fxclpl10 1 a 2'
      NAME = 'CmdLine   '
      TRIM_NAME = .true.

      CALL SUB

      END


      SUBROUTINE SUB()

      IMPLICIT NONE

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT


      character(2049)      :: CmdLine
      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i

      COMMON /args/CmdLine, NAME, TRIM_NAME

    !$OMP  PARALLEL          &
    !$OMP  SHARED(/args/)    &
    !$OMP  PRIVATE(COMMAND)  &
    !$OMP  PRIVATE(LENGTH)   &
    !$OMP  PRIVATE(STATUS)   &
    !$OMP  PRIVATE(NUMBER)   &
    !$OMP  PRIVATE(VALUE)    &
    !$OMP  PRIVATE(ARGCOUNT) &
    !$OMP  PRIVATE(Argument) &
    !$OMP  PRIVATE(i)

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
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

    !$OMP END PARALLEL

      END SUBROUTINE

      INCLUDE 'cmdline.include'

