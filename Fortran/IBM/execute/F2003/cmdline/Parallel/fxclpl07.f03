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
!*  DESCRIPTION                : Call command line intrinsic routines  through multiple parallel sections
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclpl07

      IMPLICIT NONE


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      character(2049)              :: CmdLine  = 'fxclpl07 efwerf ierjflweirjx olierufoi iewqrfu'
      integer                      :: CmdCount /4/, i, k
      character(2047)              :: Argument




    !$OMP PARALLEL SECTIONS &
    !$OMP  FIRSTPRIVATE(CmdLine) &
    !$OMP  PRIVATE(COMMAND) &
    !$OMP  PRIVATE(LENGTH) &
    !$OMP  PRIVATE(STATUS) &
    !$OMP  PRIVATE(NUMBER) &
    !$OMP  PRIVATE(VALUE) &
    !$OMP  PRIVATE(NAME) &
    !$OMP  PRIVATE(TRIM_NAME) &
    !$OMP  PRIVATE(ARGCOUNT) &
    !$OMP  PRIVATE(Argument)

    !$omp section
      do k=1, 2
        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 4 ) &
        then
          error stop 63
        endif
      end do

    !$omp section
      do k=1, 2
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif
      end do

    !$omp section
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

    !$omp section

        NAME = 'CmdLine    '
        TRIM_NAME = .true.

      do k=1, 2
        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif
      end do

     !$OMP END PARALLEL SECTIONS



      END

      INCLUDE 'cmdline.include'

