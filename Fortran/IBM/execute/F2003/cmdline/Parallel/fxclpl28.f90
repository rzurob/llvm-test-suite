! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl28 -=ert erty-=ery ewrt--= ewrtwert"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl28
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl28.f
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
!*                             : construct with lastprivate and  deduction clause
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclpl28

      IMPLICIT NONE


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT


      character(2049)      :: CmdLine  = 'fxclpl28 -=ert erty-=ery ewrt--= ewrtwert'
      integer              :: CmdCount, i, k, Reduct
      character(2047)      :: Argument


      Reduct = 0
    !$OMP  PARALLEL DO  &
    !$OMP  REDUCTION(+ : Reduct)
      DO k = 1, 5

        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 4 ) &
        then
          error stop 63
        endif

        Reduct = Reduct + 1
      END DO
    !$OMP END PARALLEL DO
      IF ( Reduct .ne. 5 ) error stop 73

      NAME = 'CmdLine    '
      TRIM_NAME = .true.
      Reduct = 0

    !$OMP  PARALLEL  DO     &
    !$OMP  PRIVATE(COMMAND) &
    !$OMP  PRIVATE(LENGTH)  &
    !$OMP  PRIVATE(STATUS)  &
    !$OMP  REDUCTION(+ : Reduct)
      DO k = 1, 5

        call GET_COMMAND(COMMAND, LENGTH, STATUS)

        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif

      Reduct = Reduct + 1
     END DO
    !$OMP END PARALLEL DO
     IF ( Reduct .ne. 5 ) error stop 74

       Reduct = 0
    !$OMP  PARALLEL  DO          &
    !$OMP  PRIVATE(LENGTH)       &
    !$OMP  PRIVATE(STATUS)       &
    !$OMP  PRIVATE(NUMBER)       &
    !$OMP  PRIVATE(VALUE)        &
    !$OMP  FIRSTPRIVATE(CmdLine) &
    !$OMP  PRIVATE(Argument)     &
    !$OMP  REDUCTION(+: Reduct)
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
          Reduct = Reduct + 1
      END DO
    !$OMP END PARALLEL  DO
     IF ( Reduct .ne. 5 ) error stop 75


       Reduct = 0
    !$OMP PARALLEL  DO             &
    !$OMP  FIRSTPRIVATE(NAME)      &
    !$OMP  FIRSTPRIVATE(TRIM_NAME) &
    !$OMP  PRIVATE(LENGTH)         &
    !$OMP  PRIVATE(STATUS)         &
    !$OMP  PRIVATE(VALUE)          &
    !$OMP  REDUCTION(+: Reduct)
      DO k = 1, 5
         call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)

        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))   .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
        then
           error stop 66
        endif
        Reduct = Reduct + 1

      END DO
    !$OMP END PARALLEL  DO
       IF ( Reduct .ne. 5 ) error stop 76


      END

      INCLUDE 'cmdline.include'



