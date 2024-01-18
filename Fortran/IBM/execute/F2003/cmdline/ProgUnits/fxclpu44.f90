! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpu44 1 a 2 b 3"
! %COMPOPTS:  -qfree=f90 -qNOSAVE
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpu44
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpu44.f
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
!*  DESCRIPTION                : Invoke command line procedures through pure 
!*                             : recursive procedures
!*                             : 
!*                             : 
!*                             : 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      MODULE MOD

      TYPE CMD

        character(50)    :: COMMAND /'??????????????????????'/
        integer          :: LENGTH  /123/
        character(50)    :: CmdLine /'fxclpu44 1 a 2 b 3'/
        integer          :: CmdCount /5/

        integer          :: STATUS  /321/
        integer          :: NUMBER  /111/
        character(50)    :: VALUE   /'WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWwwww'/

        character(50)    :: NAME    /'CmdLine  '/
        logical          :: TRIM_NAME /.true./


        character(50)    :: Argument
 
      END TYPE


      END MODULE



      PROGRAM fxclpu44

      USE MOD
      IMPLICIT NONE


      INTERFACE 

        PURE RECURSIVE LOGICAL FUNCTION PF_GET_CMD(Num)
          INTEGER, INTENT(IN) :: Num
        END FUNCTION

        PURE RECURSIVE LOGICAL FUNCTION PF_GET_CMD_ARG(Num)
          INTEGER, INTENT(IN) :: Num
        END FUNCTION

        PURE RECURSIVE LOGICAL FUNCTION PF_GET_ENV_VAR(Num)
          INTEGER, INTENT(IN) :: Num
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
        LJunk(I) = PF_GET_CMD(5) 
      END FORALL


       if ( ANY(LJunk .eqv. .false. ) ) & 
       then
         error stop 74
       endif


       FORALL (I = 1:10, NumOfExec(I) .eqv. .true.) 
         LJunk(I) = PF_GET_CMD_ARG(5)
       END FORALL


       if ( ANY(LJunk .eqv. .false. ) ) & 
       then
         error stop 75
       endif

       FORALL (I = 1:10, NumOfExec(I) .eqv. .true.) 
         LJunk(I) = PF_GET_ENV_VAR(5) 
       END FORALL


       if ( ANY(LJunk .eqv. .false. ) ) & 
       then
         error stop 76
       endif



      END 


      PURE RECURSIVE FUNCTION PF_GET_CMD(Num)

      USE MOD

      LOGICAL PF_GET_CMD
      TYPE(CMD), AUTOMATIC  ::  A
      INTEGER, INTENT(IN)   :: Num

      integer   :: i, j
 
      PF_GET_CMD = .true.

      IF (Num .gt. 1 ) THEN
        PF_GET_CMD = PF_GET_CMD(Num - 1)
      ELSE

      call GET_COMMAND(A%COMMAND, A%LENGTH, A%STATUS)
      if ( (TRIM(A%COMMAND) .ne. TRIM(A%CmdLine))  .or. &
           (A%LENGTH .ne. LEN(TRIM(A%CmdLine)))    .or. &
           (A%STATUS .ne. 0) )                          &
      then 
        PF_GET_CMD = .false. 
        ! error stop 64
      endif

      END IF

      END FUNCTION 


      PURE RECURSIVE FUNCTION PF_GET_CMD_ARG(Num) 

      USE MOD

      LOGICAL             ::  PF_GET_CMD_ARG
      INTEGER, INTENT(IN) ::  Num

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


      IF (Num .gt. 1 ) THEN
        PF_GET_CMD_ARG = PF_GET_CMD_ARG(Num - 1)
      ELSE

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

      END IF

      END FUNCTION



      PURE RECURSIVE FUNCTION PF_GET_ENV_VAR(Num) 

      USE MOD

      LOGICAL PF_GET_ENV_VAR
      TYPE(CMD)           :: A
      INTEGER, INTENT(IN) ::  Num

      integer   :: i, j
 
      PF_GET_ENV_VAR = .true.

      IF (Num .gt. 1 ) THEN
        PF_GET_ENV_VAR = PF_GET_ENV_VAR(Num - 1)
      ELSE

      call GET_ENVIRONMENT_VARIABLE(A%NAME, A%VALUE, A%LENGTH, A%STATUS, A%TRIM_NAME)
      if ( (TRIM(A%VALUE) .ne. TRIM(A%CmdLine))   .or. &
            (A%LENGTH .ne. LEN(TRIM(A%CmdLine)))  .or. &
            (A%STATUS .ne. 0))                         &
      then
         PF_GET_ENV_VAR = .false.
         ! error stop 66
      endif

      END IF

      END FUNCTION
 
      INCLUDE 'cmdline.include'

