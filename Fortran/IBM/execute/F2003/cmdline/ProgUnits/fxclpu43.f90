! *********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                : Invoke command line procedures through pure procedures
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      MODULE MOD

      TYPE CMD

        character(50)    :: COMMAND /'??????????????????????'/
        integer          :: LENGTH  /123/
        character(50)    :: CmdLine /'fxclpu43 AAAAAAA N\\@M /w/'/
        integer          :: CmdCount /3/

        integer          :: STATUS  /321/
        integer          :: NUMBER  /111/
        character(50)    :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/

        character(50)    :: NAME    /'CmdLine  '/
        logical          :: TRIM_NAME /.true./


        character(50)    :: Argument

      END TYPE


      END MODULE



      PROGRAM fxclpu43

      USE MOD
      IMPLICIT NONE


      INTERFACE

        PURE LOGICAL FUNCTION PF_GET_CMD()
        END FUNCTION

        PURE LOGICAL FUNCTION PF_GET_CMD_ARG()
        END FUNCTION

        PURE LOGICAL FUNCTION PF_GET_ENV_VAR()
        END FUNCTION

      END INTERFACE


      LOGICAL  NumOfExec(10), LJunk(10)
      INTEGER  Junk(10), I

      TYPE(CMD) A


      NumOfExec = .true.

      FORALL (I = 1:10, NumOfExec(I) .eqv. .true.)
        Junk(I) = COMMAND_ARGUMENT_COUNT()
      END FORALL


      if ( ANY(Junk .ne. A.CmdCount ) ) &
      then
        error stop 73
      endif


      FORALL (I = 1:10, NumOfExec(I) .eqv. .true.)
        LJunk(I) = PF_GET_CMD()
      END FORALL


       if ( ANY(LJunk .eqv. .false. ) ) &
       then
         error stop 74
       endif


       FORALL (I = 1:10, NumOfExec(I) .eqv. .true.)
         LJunk(I) = PF_GET_CMD_ARG()
       END FORALL


       if ( ANY(LJunk .eqv. .false. ) ) &
       then
         error stop 75
       endif

       FORALL (I = 1:10, NumOfExec(I) .eqv. .true.)
         LJunk(I) = PF_GET_ENV_VAR()
       END FORALL


       if ( ANY(LJunk .eqv. .false. ) ) &
       then
         error stop 76
       endif



      END


      PURE FUNCTION PF_GET_CMD()

      USE MOD

      LOGICAL PF_GET_CMD
      TYPE(CMD), AUTOMATIC  ::  A


      integer   :: i, j

      PF_GET_CMD = .true.

      call GET_COMMAND(A%COMMAND, A%LENGTH, A%STATUS)
      if ( (TRIM(A%COMMAND) .ne. TRIM(A%CmdLine))  .or. &
           (A%LENGTH .ne. LEN(TRIM(A%CmdLine)))    .or. &
           (A%STATUS .ne. 0) )                          &
      then
        PF_GET_CMD = .false.
        ! error stop 64
      endif

      END FUNCTION


      PURE FUNCTION PF_GET_CMD_ARG()

      USE MOD

      LOGICAL PF_GET_CMD_ARG

      INTERFACE
        pure SUBROUTINE MyGetArg(CmdLine, Num, Arg)
          CHARACTER*(*), INTENT(in)  :: CmdLine
          CHARACTER*(*), INTENT(out) ::  Arg
          INTEGER,       INTENT(in)  :: Num
        END SUBROUTINE
      END INTERFACE

      TYPE(CMD), AUTOMATIC  :: A
      integer            :: i, j

      PF_GET_CMD_ARG = .true.

      DO i  = 0, A%CmdCount

        A%NUMBER = i
        call GET_COMMAND_ARGUMENT(A%NUMBER, A%VALUE, A%LENGTH, A%STATUS)
        call MyGetArg(A%CmdLine, A%NUMBER, A%Argument)

        if ( (TRIM(A%VALUE) .ne. TRIM(A%Argument))       .or. &
             (A%LENGTH      .ne. LEN(TRIM(A%Argument)))  .or. &
             (A%STATUS      .ne. 0) )                         &
        then
          PF_GET_CMD_ARG = .false.
         ! error stop 65
        endif

      END DO

      END FUNCTION



      PURE FUNCTION PF_GET_ENV_VAR()

      USE MOD

      LOGICAL PF_GET_ENV_VAR
      TYPE(CMD)  :: A

      integer   :: i, j

      PF_GET_ENV_VAR = .true.
      call GET_ENVIRONMENT_VARIABLE(A%NAME, A%VALUE, A%LENGTH, A%STATUS, A%TRIM_NAME)
      if ( (TRIM(A%VALUE) .ne. TRIM(A%CmdLine))   .or. &
            (A%LENGTH .ne. LEN(TRIM(A%CmdLine)))  .or. &
            (A%STATUS .ne. 0))                         &
      then
         PF_GET_ENV_VAR = .false.
         ! error stop 66
      endif


      END FUNCTION

      INCLUDE 'cmdline.include'

