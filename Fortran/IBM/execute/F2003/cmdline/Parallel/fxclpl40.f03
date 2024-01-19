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
!*  DESCRIPTION                : Call command line intrinsic routines within an external recursive sub
!*                             : which is invoked within a section contruct
!*
!234567890123456789012345678901234567890123456789012345678901234567890



     RECURSIVE SUBROUTINE REC_SUB(Num)

      integer Num


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


      CmdLine = 'fxclpl40 \\^$'

      NAME = 'CmdLine   '
      TRIM_NAME = .true.




if ( Num > 1 )   &
then
   call REC_SUB( Num - 1)
else
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

end if

      END SUBROUTINE




      PROGRAM fxclp16

      IMPLICIT NONE

      integer i



   !$OMP PARALLEL SECTIONS

   !$OMP CRITICAL
          call REC_SUB(4)
   !$OMP END CRITICAL

   !$OMP SECTION
         DO i =1, 2
            call REC_SUB(3)
         END DO

   !$OMP END PARALLEL SECTIONS




      END

      INCLUDE 'cmdline.include'

