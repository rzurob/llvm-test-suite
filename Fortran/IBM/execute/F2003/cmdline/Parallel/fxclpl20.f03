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
!*  DESCRIPTION                :  Call command line intrinsic routines within an external sub
!*                             :  which is invoked through master construct
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclp20

      IMPLICIT NONE

      integer i, j, k

   !$OMP PARALLEL   DEFAULT(PRIVATE)

   !$OMP MASTER
      do i = 1, 2
          do j = 1, 2
               call sub
        end do
      end do
   !$OMP END MASTER

   !OMP$ BARRIER


      do j = 1, 1
          call sub
      end do

   !$OMP MASTER
      do k = 1, 2
          do j = 1, 2
               call sub
        end do
      end do
   !$OMP END MASTER

   !$OMP END PARALLEL

      END


      INCLUDE 'cmdline.include'



      SUBROUTINE SUB()

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


        CmdLine = 'fxclpl20 \\^--------------123 --123 \\^'
        NAME = 'CmdLine   '
        TRIM_NAME  = .true.



        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 3 ) &
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





