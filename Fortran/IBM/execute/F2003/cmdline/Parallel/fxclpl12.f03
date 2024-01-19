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
!*                             : an external subroutine and actual args are components of derived types
!*                             : in common block and  initialized in Block Data
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclpl12

      IMPLICIT NONE

      CALL SUB

      END


      SUBROUTINE SUB()

      IMPLICIT NONE

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      TYPE COM
        sequence
        character(2049)  :: CmdLine
        character(513)   :: NAME
        logical          :: TRIM_NAME
      END TYPE

      TYPE(COM)          :: ArgRec

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i

      COMMON /args/ArgRec

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
      if ( CmdCount .ne. 2 ) &
      then
        error stop 63
      endif


      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(ArgRec%CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(ArgRec%CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      !$OMP DO
      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(ArgRec%CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO
      !$OMP END DO


      call GET_ENVIRONMENT_VARIABLE(ArgRec%NAME, VALUE, LENGTH, STATUS, ArgRec%TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(ArgRec%CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(ArgRec%CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         error stop 66
      endif

    !$OMP END PARALLEL

      END SUBROUTINE

      INCLUDE 'cmdline.include'


      BLOCK DATA

      character(513)   :: NAME
      logical          :: TRIM_NAME
      character(2049)      :: CmdLine

      COMMON /args/CmdLine, NAME, TRIM_NAME

      DATA CmdLine/'fxclpl12 =================\\^ \\&'/, NAME /'CmdLine   '/, TRIM_NAME /.true./


      END BLOCK DATA


