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
!*  DESCRIPTION                : Call command line intrinsic routines through critical sections
!*                             : within  nested do while constructs in an internal procedure
!*
!234567890123456789012345678901234567890123456789012345678901234567890



      PROGRAM fxclpl09

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

      character(2049)              :: CmdLine  = 'fxclpl09 \\& \\& \\&- ----\\&'
      integer                      :: CmdCount, i, k, l, m, n
      character(2047)              :: Argument

    !$OMP  PARALLEL              &
    !$OMP  FIRSTPRIVATE(CmdLine) &
    !$OMP  PRIVATE(COMMAND)      &
    !$OMP  PRIVATE(LENGTH)       &
    !$OMP  PRIVATE(STATUS)       &
    !$OMP  PRIVATE(NUMBER)       &
    !$OMP  PRIVATE(VALUE)        &
    !$OMP  PRIVATE(NAME)         &
    !$OMP  PRIVATE(TRIM_NAME)    &
    !$OMP  PRIVATE(ARGCOUNT)     &
    !$OMP  PRIVATE(Argument)     &
    !$OMP  PRIVATE(k, l, m, n)

      k = 1
      do while (k .le. 2 )
      l=1
      do while (l .le. 2 )
      m = 1
      do while (m .le. 2 )
      n = 1
      do while (n .le. 2 )
    !$OMP CRITICAL
        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 4 ) &
        then
          error stop 63
        endif
    !$OMP END CRITICAL
      n =  n + 1
      end do
      m =  m + 1
      end do
      l = l + 1
      end do
      k = k + 1
      end do


      k = 1
      do while (k .le. 2 )
      l=1
      do while (l .le. 2 )
      m = 1
      do while (m .le. 2 )
      n = 1
      do while (n .le. 2 )
    !$OMP CRITICAL
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then

          error stop 64
        endif
    !$OMP END CRITICAL
      n =  n + 1
      end do
      m =  m + 1
      end do
      l = l + 1
      end do
      k = k + 1
      end do


      k = 1
      do while (k .le. 2 )
      l=1
      do while (l .le. 2 )
      m = 1
      do while (m .le. 2 )
      n = 1
      do while (n .le. 2 )
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
      n =  n + 1
      end do
      m =  m + 1
      end do
      l = l + 1
      end do
      k = k + 1
      end do


      k = 1
      do while (k .le. 2 )
      l=1
      do while (l .le. 2 )
      m = 1
      do while (m .le. 2 )
      n = 1
      do while (n .le. 2 )
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
      n =  n + 1
      end do
      m =  m + 1
      end do
      l = l + 1
      end do
      k = k + 1
      end do


     !$OMP END PARALLEL

      END SUBROUTINE


      END

      INCLUDE 'cmdline.include'



