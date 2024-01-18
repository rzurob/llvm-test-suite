! *********************************************************************
! %START
! %MAIN: YES
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl22 \^--------------123 --123 \^"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline_pthrd.sh fxclpl22
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl22.f
!*  TEST CASE TITLE            : Command Line Intrinsic Procedures
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct 1, 2003
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
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
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :  Call command line intrinsic routines from threads
!*                             :    
!*          
!*                     
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclp22

      use f_pthread
      
      IMPLICIT NONE

      integer i, iRC

      EXTERNAL SUB

      type(f_pthread_t) thr

      do i = 1, 20
        iRC = f_pthread_create(thr, flag=FLAG_DEFAULT, ent=SUB, arg=0)
        if ( iRC .ne. 0 ) then
          print *, "Cannot create thread #", i, " due to err=", iRC
          error stop 60
        end if
      end do



      END 

 
      INCLUDE 'cmdline.include'



      SUBROUTINE SUB( iDummy )

      use f_pthread
      
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
     

        CmdLine = 'fxclpl22 \\^--------------123 --123 \\^'
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

        call f_pthread_exit()


      END SUBROUTINE


