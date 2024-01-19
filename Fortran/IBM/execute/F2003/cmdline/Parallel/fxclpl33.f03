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
!*  DESCRIPTION                : Call command line intrinsic routines through nested parallel region
!*                             : mixed with single and critical directives
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclpl33

      IMPLICIT NONE


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT


      character(2049)      :: CmdLine  = 'fxclpl33 -wwerwe\\!qwefw wfwe\$asdf ______'
      integer              :: CmdCount, i, k
      character(2047)      :: Argument

      NAME = 'CmdLine    '
      TRIM_NAME = .true.


    !$OMP  PARALLEL
     DO k =1, 6

    !$OMP  PARALLEL                &
    !$OMP  PRIVATE(COMMAND)        &
    !$OMP  PRIVATE(LENGTH)         &
    !$OMP  PRIVATE(STATUS)         &
    !$OMP  PRIVATE(NUMBER)         &
    !$OMP  PRIVATE(VALUE)          &
    !$OMP  FIRSTPRIVATE(CmdLine)   &
    !$OMP  FIRSTPRIVATE(NAME)      &
    !$OMP  FIRSTPRIVATE(TRIM_NAME) &
    !$OMP  PRIVATE(Argument)

    !$OMP SINGLE
        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 3 ) &
        then
          error stop 63
        endif
    !$OMP END SINGLE

    !$OMP CRITICAL
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))      .or. &
           (STATUS .ne. 0) )                          &
        then
          error stop 64
        endif
    !$OMP END CRITICAL

    !$OMP BARRIER

    !$OMP MASTER
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
    !$OMP END MASTER

    !$OMP BARRIER

    !$OMP CRITICAL
        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)

        if ( (TRIM(VALUE) .ne. TRIM(CmdLine)) .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
        then
           error stop 66
        endif
    !$OMP END CRITICAL


    !$OMP END PARALLEL

    END DO
    !$OMP  END PARALLEL

      END

      INCLUDE 'cmdline.include'
