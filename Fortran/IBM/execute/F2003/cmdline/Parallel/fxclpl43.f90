! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl43 \1 \1"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl43
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl43.f
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
!*  DESCRIPTION                :  Call command line intrinsic routines in parallel do with args defined as
!*                             :  components of derived type and specified in a lastprivate clause
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclp43

      IMPLICIT NONE


      TYPE DerC
        character(513)   :: NAME
        logical          :: TRIM_NAME
        character(2049)  :: CmdLine
      END TYPE

      TYPE DerT
        character(2049)  :: COMMAND
        integer          :: LENGTH
        integer          :: STATUS
        integer          :: NUMBER
        character(2047)  :: VALUE
        integer          :: ARGCOUNT
      END TYPE

      TYPE(DerT)  D
      TYPE(DerC)  C

      integer                   :: CmdCount
      integer                   :: i, j, k
      character(2047)           :: Argument

      C%CmdLine = 'fxclpl43 \\1 \\1 '
      C%NAME = 'CmdLine   '
      C%TRIM_NAME = .true.


   !$OMP PARALLEL DO          &
   !$OMP DEFAULT(PRIVATE)     &
   !$OMP LASTPRIVATE(CmdCount)
      do i = 1, 4
             CmdCount = COMMAND_ARGUMENT_COUNT()
         if ( CmdCount .ne. 2 ) &
         then
           error stop 63
         endif
        end do
   !$OMP END PARALLEL DO
   ! Get CmdCOunt from the last  iteration

   !$OMP PARALLEL DO            &
   !$OMP DEFAULT(PRIVATE)       &
   !$OMP FIRSTPRIVATE(CmdCount) &
   !$OMP SHARED ( C )
        DO i  = 0, CmdCount
          D%NUMBER = i
          call GET_COMMAND_ARGUMENT(D%NUMBER, D%VALUE, D%LENGTH, D%STATUS)
          call MyGetArg(C%CmdLine, D%NUMBER, Argument)
          if ( (TRIM(D%VALUE) .ne. TRIM(Argument))       .or. &
               (D%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
               (D%STATUS      .ne. 0) )                       &
          then
            error stop 65
          endif
        END DO
   !$OMP END PARALLEL DO


   !$OMP PARALLEL          &
   !$OMP DEFAULT(PRIVATE)  &
   !$OMP SHARED ( C )
       call GET_COMMAND(D%COMMAND, D%LENGTH, D%STATUS)

       if ( (TRIM(D%COMMAND) .ne. TRIM(C%CmdLine))  .or. &
            (D%LENGTH .ne. LEN(TRIM(C%CmdLine)))    .or. &
            (D%STATUS .ne. 0) )                          &
       then
         error stop 64
       endif
   !$OMP END PARALLEL


   !$OMP PARALLEL          &
   !$OMP DEFAULT(PRIVATE)  &
   !$OMP SHARED ( C )
       call GET_ENVIRONMENT_VARIABLE(C%NAME, D%VALUE, D%LENGTH, D%STATUS, C%TRIM_NAME)
       if ( (TRIM(D%VALUE) .ne. TRIM(C%CmdLine))  .or. &
            (D%LENGTH .ne. LEN(TRIM(C%CmdLine)))  .or. &
            (D%STATUS .ne. 0))                         &
       then
         error stop 66
       endif
   !$OMP END PARALLEL


      END

      INCLUDE 'cmdline.include'

!
