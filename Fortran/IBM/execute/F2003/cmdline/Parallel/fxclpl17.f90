! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl17 o-o-o-o-o-o -0-0-0-0-0-0"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl17
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl17.f
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
!*                             :  which is invoked through a section contruct
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclp17

      IMPLICIT NONE

      integer i



   !$OMP PARALLEL SECTIONS DEFAULT(PRIVATE)

   !$OMP SECTION
      do i = 1, 3
          call sub
      end do

   !$OMP SECTION
     call sub

   !$OMP SECTION
      do i = 1, 2
          call sub
      end do


   !$OMP END PARALLEL SECTIONS

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


        CmdLine = 'fxclpl17 o-o-o-o-o-o -0-0-0-0-0-0'
        NAME = 'CmdLine   '
        TRIM_NAME  = .true.



        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 2 ) &
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

