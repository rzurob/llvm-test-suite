! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl08 dsfgdtg rewt ----\&"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl08
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl08.f
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
!*  DESCRIPTION                : Call command line intrinsic routines through critical sections
!*                             : within  nested do loops in an internal procedure
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclpl08

      IMPLICIT NONE

      call sub

      contains

      SUBROUTINE SUB()


      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      character(2049)              :: CmdLine  = 'fxclpl08 dsfgdtg rewt ----\\&'
      integer                      :: CmdCount, i, k, l, m, n
      character(2047)              :: Argument

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

      do k =1, 2
      do l =1, 2
      do m =1, 2
      do n =1, 2
    !$OMP CRITICAL
        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 3 ) &
        then
          error stop 63
        endif
    !$OMP END CRITICAL
      end do
      end do
      end do
      end do


      do k =1, 2
      do l =1, 2
      do m =1, 2
      do n =1, 2
    !$OMP CRITICAL
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif
    !$OMP END CRITICAL
      end do
      end do
      end do
      end do


      do k =1, 2
      do l =1, 2
      do m =1, 2
      do n =1, 2
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
      end do
      end do
      end do
      end do


      do k =1, 2
      do l =1, 2
      do m =1, 2
      do n =1, 2
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
      end do
      end do
      end do


     !$OMP END PARALLEL

      END SUBROUTINE


      END

      INCLUDE 'cmdline.include'



