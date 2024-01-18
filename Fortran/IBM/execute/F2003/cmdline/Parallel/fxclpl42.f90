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
!*  DESCRIPTION                : Call command line intrinsic routines   in do loop with INDEPENDENT
!*                             : directive within parallel section
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclpl42

      IMPLICIT NONE


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      character(2049)              :: CmdLine  = 'fxclpl42 ======000000000000======= +++1 aaa-----------++'
      integer                      :: CmdCount, i, k
      character(2047)              :: Argument




    !$OMP  PARALLEL              &
    !$OMP  FIRSTPRIVATE(CmdLine) &
    !$OMP  PRIVATE(COMMAND)      &
    !$OMP  PRIVATE(LENGTH)       &
    !$OMP  PRIVATE(STATUS)       &
    !$OMP  PRIVATE(NUMBER)       &
    !$OMP  PRIVATE(VALUE)        &
    !$OMP  PRIVATE(NAME)         &
    !$OMP  PRIVATE(TRIM_NAME)    &
    !$OMP  PRIVATE(ARGCOUNT)     &
    !$OMP  PRIVATE(Argument)

    do k=1, 2
      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
      then
        error stop 63_4
      endif
    end do


    !$OMP  SECTIONS
    !$omp section

    !$omp section
    !$OMP INDEPENDENT, NEW(COMMAND, LENGTH, STATUS)
      do k=1, 2
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64_4
        endif
      end do

    !$omp section
    !$OMP INDEPENDENT, NEW(NUMBER, VALUE, LENGTH, STATUS)
        DO i  = 0, CmdCount

          NUMBER = i
          call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
          call MyGetArg(CmdLine, NUMBER, Argument)
          if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
               (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
               (STATUS      .ne. 0) )                       &
          then
            error stop 65_4
          endif

        END DO

    !$omp section
    !$OMP INDEPENDENT, NEW(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      do k=1, 2
        NAME = 'CmdLine    '
        TRIM_NAME = .true.
        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66_4
        endif
      end do

     !$OMP END SECTIONS
     !$OMP END PARALLEL



      END

      INCLUDE 'cmdline.include'

