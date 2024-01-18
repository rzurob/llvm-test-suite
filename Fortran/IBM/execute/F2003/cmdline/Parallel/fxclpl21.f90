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
!*  DESCRIPTION                :  Call command line intrinsic routines within an ordered construct
!*                             :  in a parallel region
!*
!234567890123456789012345678901234567890123456789012345678901234567890




      PROGRAM fxclpl21

      IMPLICIT NONE

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      integer          :: ARGCOUNT

      integer              :: CmdCount
      character(2047)      :: Argument
      integer              :: i,k

      character(513)   :: NAME
      logical          :: TRIM_NAME
      character(2049)  :: CmdLine

      COMMON /args0/CmdLine, NAME, TRIM_NAME
      COMMON /args1/COMMAND, LENGTH, STATUS, NUMBER, VALUE, ARGCOUNT


      CmdLine = 'fxclpl21 %1 %2 %3'
      NAME = 'CmdLine   '
      TRIM_NAME  = .true.


    !$OMP  PARALLEL DO ORDERED      &
    !$OMP  SHARED(/args0/)   &
    !$OMP  PRIVATE(/args1/)  &
    !$OMP  PRIVATE(CmdCount) &
    !$OMP  PRIVATE(Argument) &
    !$OMP  PRIVATE(i)

     DO k = 1, 4

      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
      then
        error stop 63
      endif

      IF ( k .eq. 1) THEN
     !$OMP ORDERED
      call GET_COMMAND(COMMAND, LENGTH, STATUS)
      if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
           (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (STATUS .ne. 0) )                        &
      then
        error stop 64
      endif
     !$OMP END ORDERED
      END IF

      IF ( k .eq. 2 ) THEN
     !$OMP ORDERED
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
     !$OMP END ORDERED
      END IF


      IF ( k .ge. 3) THEN
     !$OMP ORDERED
      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
      if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
            (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
            (STATUS .ne. 0))                       &
      then
         error stop 66
      endif
     !$OMP END ORDERED
      END IF

    END DO

    !$OMP END PARALLEL DO

    END



      INCLUDE 'cmdline.include'



