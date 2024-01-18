! *********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                : Call command line intrinsic routines through routines
!*                             : with interface generic name the same as the related intrinsic routine name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      character(513)   :: NAME
      logical          :: TRIM_NAME
      character(2049)  :: CmdLine


      DATA CmdLine    /'fxcllf25 1 a 2'/
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



      PROGRAM fxcllf25

      USE MOD

      LOGICAL L

      INTRINSIC COMMAND_ARGUMENT_COUNT
      INTRINSIC GET_COMMAND
      INTRINSIC GET_COMMAND_ARGUMENT
      INTRINSIC GET_ENVIRONMENT_VARIABLE


      INTERFACE COMMAND_ARGUMENT_COUNT
        FUNCTION F_COMMAND_ARGUMENT_COUNT(NoUse)
        LOGICAL F_COMMAND_ARGUMENT_COUNT
        LOGICAL NoUse
        END FUNCTION
      END INTERFACE

      INTERFACE GET_COMMAND
        SUBROUTINE S_GET_COMMAND(NoUse)
        LOGICAL NoUse
        END SUBROUTINE
      END INTERFACE

      INTERFACE GET_COMMAND_ARGUMENT
        SUBROUTINE S_GET_COMMAND_ARGUMENT(NoUse)
        LOGICAL NoUse
        END SUBROUTINE
      END INTERFACE

      INTERFACE GET_ENVIRONMENT_VARIABLE
        SUBROUTINE S_GET_ENVIRONMENT_VARIABLE(NoUse)
        LOGICAL NoUse
        END SUBROUTINE
      END INTERFACE



      L = F_COMMAND_ARGUMENT_COUNT(.true.)

      CALL GET_COMMAND(.true.)

      CALL GET_COMMAND_ARGUMENT(.true.)

      CALL GET_ENVIRONMENT_VARIABLE(.true.)


      END


      FUNCTION F_COMMAND_ARGUMENT_COUNT(NoUse)

      USE MOD
      LOGICAL F_COMMAND_ARGUMENT_COUNT
      LOGICAL NoUse

        F_COMMAND_ARGUMENT_COUNT = .true.
        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 3 ) &
        then
          error stop 63
        endif

      END FUNCTION



      SUBROUTINE S_GET_COMMAND()

      USE MOD

        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif

      END SUBROUTINE


      SUBROUTINE S_GET_COMMAND_ARGUMENT()

      USE MOD

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


      SUBROUTINE S_GET_ENVIRONMENT_VARIABLE()

      USE MOD

        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif

      ENDSUBROUTINE



      INCLUDE 'cmdline.include'
