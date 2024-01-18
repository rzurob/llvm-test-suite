! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxcllf28 1 a 2 b"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxcllf28
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxcllf28.f
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
!*  DESCRIPTION                : Use command line intrinsic routine names as if construct name
!*                             : and call these intrinsics in functions
!*                             : Use labels as actual arguments to return to  if constructs
!*   28. Use procedures and  labels as actual args to have alternative returns
!234567890123456789012345678901234567890123456789012345678901234567890

      MODULE MOD

      character(513)   :: NAME
      logical          :: TRIM_NAME
      character(2049)  :: CmdLine


      DATA CmdLine    /'fxcllf28 1 a 2 b'/
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



      PROGRAM fxcllf28

      USE MOD

      LOGICAL L

      INTERFACE
           FUNCTION  F_COMMAND_ARGUMENT_COUNT()
           LOGICAL F_COMMAND_ARGUMENT_COUNT
           END FUNCTION

           FUNCTION  F_GET_COMMAND()
           LOGICAL F_GET_COMMAND
           END FUNCTION

           FUNCTION  F_GET_COMMAND_ARGUMENT()
           LOGICAL F_GET_COMMAND_ARGUMENT
           END FUNCTION

           FUNCTION   F_GET_ENVIRONMENT_VARIABLE()
           LOGICAL F_GET_ENVIRONMENT_VARIABLE
           END FUNCTION

      END INTERFACE


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!   NAME: IF (logical expression) THEN
!            action-statement
!          ENDIF NAME
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        CALL S_COMMAND_ARGUMENT_COUNT(*10)

10     COMMAND_ARGUMENT_COUNT:  if ( F_COMMAND_ARGUMENT_COUNT()) then
                L = F_GET_COMMAND()
            end if  COMMAND_ARGUMENT_COUNT

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!   NAME: IF (logical expression) THEN
!            action-statement
!          ELSE [NAME]
!            action-statment
!          ENDIF NAME
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

         CALL S_COMMAND_ARGUMENT_COUNT(*20)

20     GET_COMMAND_ARGUMENT:  if ( F_COMMAND_ARGUMENT_COUNT()) then
              L = F_GET_COMMAND()
            else
              L = F_GET_COMMAND_ARGUMENT()
            end if GET_COMMAND_ARGUMENT


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!   NAME: IF (logical expression) THEN
!            action-statement
!          ELSEIF (logical expression) THEN [NAME]
!            action-statement
!          ENDIF NAME
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        CALL S_GET_COMMAND_ARGUMENT(*30)

30     GET_ENVIRONMENT_VARIABLE: if ( F_GET_ENVIRONMENT_VARIABLE() ) then
              L = F_GET_ENVIRONMENT_VARIABLE()
            elseif (F_GET_COMMAND()) then
              L = F_GET_COMMAND()
            end if GET_ENVIRONMENT_VARIABLE

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!   NAME: IF (logical expression) THEN
!            action-statement
!          ELSEIF (logical expression) THEN [NAME]
!            action-statement
!          ELSE [NAME]
!            action-statment
!          ENDIF NAME
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

         CALL S_GET_COMMAND(*40)

40      GET_COMMAND: IF (F_COMMAND_ARGUMENT_COUNT()     .and. &
        F_GET_COMMAND()                               .and. &
        F_GET_COMMAND_ARGUMENT()                      .and. &
        F_GET_ENVIRONMENT_VARIABLE()   )                    &
      THEN
        L = F_COMMAND_ARGUMENT_COUNT() .and. &
        F_GET_COMMAND()                .and. &
        F_GET_COMMAND_ARGUMENT()       .and. &
        F_GET_ENVIRONMENT_VARIABLE()
      ELSEIF (F_GET_ENVIRONMENT_VARIABLE() )  THEN
        L = F_GET_ENVIRONMENT_VARIABLE()
      ELSE
        L = F_GET_COMMAND()
      ENDIF GET_COMMAND



      END


      FUNCTION F_COMMAND_ARGUMENT_COUNT()

      USE MOD
      LOGICAL F_COMMAND_ARGUMENT_COUNT
      LOGICAL NoUse

        F_COMMAND_ARGUMENT_COUNT = .true.
        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 4 ) &
        then
          F_COMMAND_ARGUMENT_COUNT = .false.
          error stop 63
        endif

      END FUNCTION



      FUNCTION F_GET_COMMAND()

      USE MOD
      LOGICAL F_GET_COMMAND

        F_GET_COMMAND = .true.
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          F_GET_COMMAND = .false.
          error stop 64
        endif

      END FUNCTION


      FUNCTION F_GET_COMMAND_ARGUMENT()

      USE MOD
      LOGICAL F_GET_COMMAND_ARGUMENT

        F_GET_COMMAND_ARGUMENT = .false.
        DO i  = 0, CmdCount
          NUMBER = i
          call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
          call MyGetArg(CmdLine, NUMBER, Argument)

          if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
               (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
               (STATUS      .ne. 0) )                       &
          then
            F_GET_COMMAND_ARGUMENT = .true.
            error stop 65
          endif
        END DO

      END FUNCTION


      FUNCTION F_GET_ENVIRONMENT_VARIABLE()

      USE MOD
      LOGICAL F_GET_ENVIRONMENT_VARIABLE

        F_GET_ENVIRONMENT_VARIABLE = .true.
        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          F_GET_ENVIRONMENT_VARIABLE = .false.
          error stop 66
        endif

      ENDFUNCTION


! SUB DEF

      SUBROUTINE S_COMMAND_ARGUMENT_COUNT(*)

      USE MOD

        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 4 ) &
        then
          error stop 63
        endif

        RETURN 1

      END SUBROUTINE



      SUBROUTINE S_GET_COMMAND(*)

      USE MOD

        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif

        RETURN 1

      END SUBROUTINE


      SUBROUTINE S_GET_COMMAND_ARGUMENT(*)

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

        RETURN 1

      END SUBROUTINE


      SUBROUTINE S_GET_ENVIRONMENT_VARIABLE(*)

      USE MOD

        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif

        RETURN 1

      END SUBROUTINE


      INCLUDE 'cmdline.include'


