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
!*  DESCRIPTION                :  Call command line intrinsic routines through parallel region
!*                             :  in an internal sub
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclp36

      IMPLICIT NONE



      integer i



   !$OMP PARALLEL
      do i = 1, 10
          call int_sub
      end do
   !$OMP END PARALLEL



     CONTAINS


      SUBROUTINE INT_SUB()

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      character(2049)  :: CmdLine

      integer                      :: CmdCount, i, k
      character(2047)              :: Argument


      CmdLine = 'fxclpl36 11 22 33 aa bb cc \\&\\& dd'

      NAME = 'CmdLine   '
      TRIM_NAME = .true.

   !$OMP PARALLEL DEFAULT(PRIVATE), FIRSTPRIVATE(CmdLine, NAME, TRIM_NAME)

   !$OMP MASTER
        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 8 ) &
        then
          error stop 63
        endif
   !$OMP END MASTER
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif

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

        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif

   !$OMP END PARALLEL

      END SUBROUTINE

      END

      INCLUDE 'cmdline.include'

