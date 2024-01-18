! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl29 ddddddd=ddddd IIIIII---IIIIIII QQQQ_____"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl29
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl29.f
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
!*  DESCRIPTION                : Call command line intrinsic routines  within forall construct
!*                             : through  parallel region
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      INCLUDE 'cmdline.include'
      MODULE MOD

      INTERFACE

      pure SUBROUTINE MyGetArg(CmdLine, Num, Arg)

      CHARACTER*(*), INTENT(in)      :: CmdLine
      CHARACTER*(*), INTENT(out)     :: Arg
      INTEGER,       INTENT(in)      :: Num
      END SUBROUTINE

      END INTERFACE

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

        DATA CmdLine/'fxclpl29 ddddddd=ddddd IIIIII---IIIIIII QQQQ_____'/, NAME /'CmdLine   '/, TRIM_NAME /.true./

      END BLOCK DATA



      PROGRAM fxclpl29

      USE MOD

      IMPLICIT NONE


      INTERFACE

        PURE LOGICAL FUNCTION GET_CMD()
        END FUNCTION

        PURE LOGICAL FUNCTION GET_CMD_ARG(iCount)
          INTEGER, intent(in)::iCOUNT
        END FUNCTION

        PURE LOGICAL FUNCTION GET_ENV_VAR()
        END FUNCTION

      END INTERFACE


      LOGICAL L(5)
      INTEGER Count(5), k


    !$OMP  PARALLEL

      FORALL ( k = 1 : 5 )
        Count(k) = COMMAND_ARGUMENT_COUNT()
      END FORALL

    !$OMP  END PARALLEL

    do k=1,5
       if (count(k) /= 3)        error stop 1
    end do

    !$OMP  PARALLEL
      FORALL ( k = 1 : 5 )
        L(k) = GET_CMD()
      END FORALL
    !$OMP  END PARALLEL

    IF (.not.any(L))  error stop 64

    !$OMP  PARALLEL

      FORALL ( k = 1 : 5 )
        L(k)= GET_CMD_ARG( COMMAND_ARGUMENT_COUNT() )
      END FORALL

    !$OMP END PARALLEL

    if (.not.any(L)) error stop 65

    !$OMP  PARALLEL

      FORALL ( k = 1: 5 )
         L(k) = GET_ENV_VAR()
      END FORALL

    !$OMP END PARALLEL



     END


      PURE FUNCTION GET_CMD()

      USE MOD

      LOGICAL GET_CMD

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT


      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      GET_CMD = .true.

      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        GET_CMD = .false.
      endif

      END FUNCTION

      PURE FUNCTION GET_CMD_ARG(CmdCount)

      USE MOD

      LOGICAL GET_CMD_ARG

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer,             intent(in):: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      GET_CMD_ARG = .true.

      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          GET_CMD_ARG = .false.

        endif

      END DO

      END FUNCTION



      FUNCTION GET_ENV_VAR()

      USE MOD

      LOGICAL GET_ENV_VAR

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i, j

      GET_ENV_VAR = .true.
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         GET_ENV_VAR = .false.
         error stop 66
      endif


      END FUNCTION






