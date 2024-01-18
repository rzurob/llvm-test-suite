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
!*                             :  with ordered clause
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclp38

      IMPLICIT NONE

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      character(2049)  :: CmdLine

      integer                      :: CmdCount, i, j, k
      character(2047)              :: Argument


      CmdLine = 'fxclpl38 -$1 -$2 -$3 -$4'
      NAME = 'CmdLine   '
      TRIM_NAME = .true.



   !$OMP PARALLEL DO ORDERED DEFAULT(PRIVATE), FIRSTPRIVATE(CmdLine, NAME, TRIM_NAME) COLLAPSE(2)


      do j = 1, 10
      do k = 1, 10

        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 4 ) &
        then
          error stop 73
        endif

        IF ( k .lt. 5) THEN
   !$OMP ORDERED
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 74
        endif
   !$OMP END ORDERED
        END IF

        DO i  = 0, CmdCount
          NUMBER = i
          call GET_COMMAND_ARGUMENT(NUMBER, VALUE, LENGTH, STATUS)
          call MyGetArg(CmdLine, NUMBER, Argument)
          if ( (TRIM(VALUE) .ne. TRIM(Argument))       .or. &
               (LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
               (STATUS      .ne. 0) )                       &
          then
            error stop 75
          endif
        END DO


        IF ( k .gt. 5 ) THEN
   !$OMP ORDERED
        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 76
        endif
   !$OMP END ORDERED
        END IF

   !$OMP FLUSH

     end do
     end do
   !$OMP END PARALLEL DO


      END

      INCLUDE 'cmdline.include'

