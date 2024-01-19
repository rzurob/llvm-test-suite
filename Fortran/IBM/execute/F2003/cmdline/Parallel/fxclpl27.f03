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
!*  DESCRIPTION                : Call command line intrinsic routines through single parallel do
!*                             : construct with lastprivate clauses
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclpl27

      IMPLICIT NONE


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT


      character(2049)      :: CmdLine  = 'fxclpl27 ++++++1111111 ooooooooooo00000000 dvce'
      integer              :: CmdCount, i, k
      character(2047)      :: Argument



      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
      then
        error stop 63
      endif

      NAME = 'CmdLine    '
      TRIM_NAME = .true.



    !$OMP  PARALLEL  DO         &
    !$OMP  LASTPRIVATE(COMMAND) &
    !$OMP  LASTPRIVATE(LENGTH)  &
    !$OMP  LASTPRIVATE(STATUS)
      DO k = 1, 5
      call GET_COMMAND(COMMAND, LENGTH, STATUS)
     END DO
    !$OMP END PARALLEL DO

      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif


      DO i  = 0, CmdCount

    !$OMP  PARALLEL  DO        &
    !$OMP  LASTPRIVATE(LENGTH) &
    !$OMP  LASTPRIVATE(STATUS) &
    !$OMP  LASTPRIVATE(NUMBER) &
    !$OMP  LASTPRIVATE(VALUE)
        DO k = 1, 5
          NUMBER = i
          call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
        END DO
    !$OMP END PARALLEL  DO

        call MyGetArg(CmdLine, NUMBER, Argument)

        if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
             (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO



    !$OMP PARALLEL  DO             &
    !$OMP  FIRSTPRIVATE(NAME)      &
    !$OMP  FIRSTPRIVATE(TRIM_NAME) &
    !$OMP  LASTPRIVATE(LENGTH)     &
    !$OMP  LASTPRIVATE(STATUS)     &
    !$OMP  LASTPRIVATE(VALUE)
      DO k = 1, 5
         call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      END DO
    !$OMP END PARALLEL  DO


      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))   .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         error stop 66
      endif


      END

      INCLUDE 'cmdline.include'



