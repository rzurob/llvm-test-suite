! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct 1, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   : COMMAND_ARGUMENT_COUNT()
!*                             : GET_COMMAND(COMMAND, LENGTH, STATUS)
!*                             : GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
!*                             : GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
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
!*  DESCRIPTION                : Call command line intrinsic routines in parallel region
!*                             : through CRITICAL constructs
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclpl04


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      character(2049)      :: CmdLine  = 'fxclpl04 ---i -i- -123234123412 _i2134123 234213_i'
      integer              :: CmdCount, i, k
      character(2047)      :: Argument


  !$OMP PARALLEL  &
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

      do k=1, 40

   !$OMP CRITICAL
        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 5 ) &
        then
          error stop 63
        endif
   !$OMP END CRITICAL

   !$OMP CRITICAL
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif
   !$OMP END CRITICAL

   !$OMP CRITICAL
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
   !$OMP END CRITICAL

   !$OMP CRITICAL
        NAME = 'CmdLine    '
        TRIM_NAME = .true.

        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif
   !$OMP END CRITICAL

      end do

 !$OMP END PARALLEL

      END

      INCLUDE 'cmdline.include'


