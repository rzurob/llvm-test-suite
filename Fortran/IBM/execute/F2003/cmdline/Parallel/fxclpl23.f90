! *********************************************************************
! %START
! %MAIN: YES
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl23 =-=-=-=-=-=-=--=-=-245324-523=-5=2321351=34-"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline_pthrd.sh fxclpl23
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl23.f
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
!*  DESCRIPTION                :  Call command line intrinsic routines from threads through mutex
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  module THREADMOD

    use f_pthread

    TYPE(f_pthread_mutex_t) mutex /PTHREAD_MUTEX_INITIALIZER/

    TYPE(f_pthread_t) THREAD

  end module THREADMOD


      PROGRAM fxclp23

      use THREADMOD

      IMPLICIT NONE

      integer i, iRC

      EXTERNAL SUB


      do i = 1, 20
        iRC = f_pthread_create(THREAD, flag=FLAG_DEFAULT, ent=SUB, arg=i)
        if ( iRC .ne. 0 ) then
          print *, "Cannot create thread #", i, " due to err=", iRC
          error stop 60
        end if
      end do


      END


      INCLUDE 'cmdline.include'



      SUBROUTINE SUB( iDummy )

      use THREADMOD

      INTEGER iDummy

      character(2049)  :: COMMAND
      integer          :: LENGTH
      integer          :: STATUS
      integer          :: NUMBER
      character(2047)  :: VALUE
      character(513)   :: NAME
      logical          :: TRIM_NAME
      integer          :: ARGCOUNT

      character(2049)  :: CmdLine
      integer          :: CmdCount, i, k
      character(2047)  :: Argument


        CmdLine = 'fxclpl23 =-=-=-=-=-=-=--=-=-245324-523=-5=2321351=34-'
        NAME = 'CmdLine   '
        TRIM_NAME  = .true.

    do while (f_pthread_mutex_trylock(mutex) .ne. 0)
    end do
        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 1 ) &
        then
          error stop 63
        endif
    if (f_pthread_mutex_unlock(mutex) .ne. 0) then
       print *, "Cannot unlock the mutex."
       error stop 73
    end if

    do while (f_pthread_mutex_trylock(mutex) .ne. 0)
    end do
      call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif
    if (f_pthread_mutex_unlock(mutex) .ne. 0) then
       print *, "Cannot unlock the mutex."
       error stop 74
    end if

    do while (f_pthread_mutex_trylock(mutex) .ne. 0)
    end do
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
    if (f_pthread_mutex_unlock(mutex) .ne. 0) then
       print *, "Cannot unlock the mutex."
       error stop 75
    end if

    do while (f_pthread_mutex_trylock(mutex) .ne. 0)
    end do
        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif

    if (f_pthread_mutex_unlock(mutex) .ne. 0) then
       print *, "Cannot unlock the mutex."
       error stop 76
    end if

        call f_pthread_exit()


      END SUBROUTINE




