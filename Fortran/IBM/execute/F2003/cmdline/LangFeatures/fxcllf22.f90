! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf22 1"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf22
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf22.f
!*
!*  DATE                       : Sept 18, 2003
!*
!*  PRIMARY SUBROUTINES TESTED   	: COMMAND_ARGUMENT_COUNT()
!*                            	: GET_COMMAND(COMMAND, LENGTH, STATUS)
!*                            	: GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
!*                             	: GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
!*
!*  SECONDARY SUBROUTINES TESTED :
!*
!*  REFERENCE                  : Feature 252525
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Call command line intrinsic routines through  a call chain of
!*                             : internal / external subrouitnes with typeless data
!*                             : (Octal/Hex) are intent(in) args
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      character(513)   :: NAME
      logical          :: TRIM_NAME
      character(2049)  :: CmdLine


      DATA CmdLine    /'fxcllf22 1'/
      DATA NAME       /'CmdLine   '/
      DATA TRIM_NAME  /.true./


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT


      DATA COMMAND    / '????? '/
      DATA LENGTH     / 1111 /
      DATA STATUS     / 1111 /
      DATA NUMBER     /2222/
      DATA VALUE      / 1*'!'/
      DATA ARGCOUNT   / 0 /



      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i


      END MODULE



      PROGRAM fxcllf22
      IMPLICIT NONE


      CALL SUB0

      CONTAINS

      SUBROUTINE SUB0


      CALL  EXT_COMMAND_ARGUMENT_COUNT( )

      CALL EXT_GET_COMMAND( )

      CALL EXT_GET_COMMAND_ARGUMENT( COMMAND_ARGUMENT_COUNT())

      CALL EXT_GET_ENVIRONMENT_VARIABLE( )


      END SUBROUTINE

      END




      SUBROUTINE EXT_COMMAND_ARGUMENT_COUNT()
      USE MOD

         CmdCount = COMMAND_ARGUMENT_COUNT()
         if ( CmdCount .ne. 1 ) &
         then
           error stop 63
         endif

      END SUBROUTINE



      SUBROUTINE EXT_GET_COMMAND()
      USE MOD

        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif

      END SUBROUTINE


      SUBROUTINE EXT_GET_COMMAND_ARGUMENT(Num)
      USE MOD
      INTEGER Num

        CmdCount = Num
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


      SUBROUTINE EXT_GET_ENVIRONMENT_VARIABLE()
      USE MOD
      INTEGER Num

      ! z'436D644C696E652020'  == 'CmdLine  '
      call GET_ENVIRONMENT_VARIABLE(z'436D644C696E652020', VALUE(1013:2039), LENGTH, STATUS, .true. .or. .true.)
      if ( (TRIM(VALUE(1013:2039)) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))             .or. &
           (STATUS .ne. 0))                                  &
      then
        error stop 70
      endif

      !o'206665442306455631220040' == 'CmdLine'
      call GET_ENVIRONMENT_VARIABLE(o'206665442306455631220040', LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=.false. .and. .true.)
      if ( (LENGTH .ne.LENGTH)  .or. &
           (STATUS .ne. STATUS))     &
      then
        error stop 71
      endif




      END SUBROUTINE




      INCLUDE 'cmdline.include'

