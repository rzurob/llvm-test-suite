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
!*  DESCRIPTION                : Call command line intrinsic routines within where construct
!*                             : in parallel region in an external sub  with workshare directive
!*                             : which is invoked by an internal  sub
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

        character(513)   :: NAME
        logical          :: TRIM_NAME
        character(2049)  :: CmdLine

        COMMON /sargs/CmdLine, NAME, TRIM_NAME

      END MODULE


      BLOCK DATA

        character(513)   :: NAME
        logical          :: TRIM_NAME
        character(2049)  :: CmdLine

        COMMON /sargs/CmdLine, NAME, TRIM_NAME

        DATA CmdLine /"fxclpl49 {A} {B] [C} {D{ }E] }O{"/,NAME /'CmdLine    '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxclpl49

      INTEGER I

      !$OMP PARALLEL DEFAULT(PRIVATE)
         DO I = 1, 5
           CALL INT_SUB
         END DO
      !$OMP END PARALLEL


      CONTAINS

      SUBROUTINE INT_SUB

           CALL EXT_SUB

      END SUBROUTINE

      END PROGRAM


      SUBROUTINE EXT_SUB

      USE MOD
      IMPLICIT NONE


      INTERFACE

        LOGICAL FUNCTION SF_GET_CMD()
        END FUNCTION

        LOGICAL FUNCTION SF_GET_CMD_ARG(iCount)
          INTEGER iCOUNT
        END FUNCTION

        LOGICAL FUNCTION SF_GET_ENV_VAR()
        END FUNCTION

      END INTERFACE


      INTEGER  CMD_ARG_COUNT
      LOGICAL  GET_CMD
      LOGICAL  GET_CMD_ARG
      LOGICAL  GET_ENV_VAR


      GET_CMD()       =  SF_GET_CMD()
      GET_CMD_ARG()   =  SF_GET_CMD_ARG(COMMAND_ARGUMENT_COUNT())
      GET_ENV_VAR()   =  SF_GET_ENV_VAR()


      LOGICAL  NumOfExec(10), LJunk(10)
      INTEGER  Junk(10)


     NumOfExec = .true.

    !$OMP  PARALLEL

    !$OMP WORKSHARE
        WHERE (NumOfExec .eqv. .true.)
        Junk = COMMAND_ARGUMENT_COUNT()
        END WHERE
    !$OMP END WORKSHARE

        if ( ANY(Junk .ne. 6 ) ) &
        then
          error stop 73
        endif


    !$OMP WORKSHARE
       WHERE (NumOfExec .eqv. .true.)
       LJunk = GET_CMD()
       END WHERE
    !$OMP END WORKSHARE

       if ( ANY(LJunk .eqv. .false. ) ) &
       then
         error stop 74
       endif

    !$OMP WORKSHARE
       WHERE (NumOfExec .eqv. .true.)
       LJunk = GET_CMD_ARG()
       END WHERE
    !$OMP END WORKSHARE

       if ( ANY(LJunk .eqv. .false. ) ) &
       then
         error stop 75
       endif

    !$OMP WORKSHARE
       WHERE (NumOfExec .eqv. .true.)
       LJunk = GET_ENV_VAR()
       END WHERE
    !$OMP END WORKSHARE

       if ( ANY(LJunk .eqv. .false. ) ) &
       then
         error stop 76
       endif

    !$OMP END PARALLEL

      END SUBROUTINE


      FUNCTION SF_GET_CMD()

      USE MOD

      LOGICAL SF_GET_CMD

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT


      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_CMD = .true.

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        SF_GET_CMD = .false.
        ! error stop 64
      endif

      END FUNCTION

      FUNCTION SF_GET_CMD_ARG(CmdCount)

      USE MOD

      LOGICAL SF_GET_CMD_ARG

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_CMD_ARG = .true.

      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          SF_GET_CMD_ARG = .false.
         ! error stop 65
        endif

      END DO

     END FUNCTION



      FUNCTION SF_GET_ENV_VAR()

      USE MOD

      LOGICAL SF_GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      SF_GET_ENV_VAR = .true.
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         SF_GET_ENV_VAR = .false.
         ! error stop 66
      endif


      END FUNCTION



      INCLUDE 'cmdline.include'


