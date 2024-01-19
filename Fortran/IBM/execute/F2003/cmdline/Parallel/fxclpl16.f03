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
!*  DESCRIPTION                :  Call command line intrinsic routines within an suboutine in a module
!*                             :  which is invoked through parallel region with actual args within common blocks
!*
!234567890123456789012345678901234567890123456789012345678901234567890



     MODULE MOD


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      character(2049)  :: CmdLine

      COMMON /sargs/CmdLine, NAME, TRIM_NAME
      COMMON /pargs/ COMMAND, LENGTH, STATUS, NUMBER, VALUE, ARGCOUNT


   !$OMP THREADPRIVATE(/pargs/)


     CONTAINS


      SUBROUTINE SUB()

      integer                      :: CmdCount, i, k
      character(2047)              :: Argument


        CmdLine = 'fxclpl16 \\^\\&'
        NAME = 'CmdLine   '
        TRIM_NAME  = .true.



        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 1 ) &
        then
          error stop 63
        endif

        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif

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



        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif


      END SUBROUTINE

     END MODULE



      PROGRAM fxclp16

      USE MOD
      IMPLICIT NONE

      integer i



   !$OMP PARALLEL DO
      do i = 1, 10
          call sub
      end do
   !$OMP END PARALLEL DO




      END

      INCLUDE 'cmdline.include'




