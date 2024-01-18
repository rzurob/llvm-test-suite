! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=5;unset XLSMPOPTS; export XLSMPOPTS=SCHEDULE=DYNAMIC:PARTHDS=4 ;export CmdLine="fxclpl52 -i -i -i -i"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl52
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl52.f
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
!*  DESCRIPTION                : Set and read XLSMPOPTS in parallel environment
!*                             : Call command line intrinsic routines  through multiple parallel sections
!*                               
!*                                        
!*                          
!234567890123456789012345678901234567890123456789012345678901234567890

 

      PROGRAM fxclpl52
      
      IMPLICIT NONE


      character(2049)  :: COMMAND
      integer          :: LENGTH     
      integer          :: STATUS  
      integer          :: NUMBER 
      character(2047)  :: VALUE  
      character(513)   :: NAME  
      logical          :: TRIM_NAME 
      integer          :: ARGCOUNT 

      character(2049)              :: CmdLine  = 'fxclpl52 -i -i -i -i'
      integer                      :: CmdCount/4/, i, k
      character(2047)              :: Argument



 
    !$OMP PARALLEL SECTIONS &
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

    !$omp section
      do k=1, 2 
        CmdCount = COMMAND_ARGUMENT_COUNT()
        if ( CmdCount .ne. 4 ) & 
        then
          error stop 63
        endif

        call GET_ENVIRONMENT_VARIABLE('XLSMPOPTS', VALUE, LENGTH, STATUS, .true.)
        if (TRIM(VALUE) .ne. 'SCHEDULE=DYNAMIC:PARTHDS=4') ERROR STOP 73

      end do

    !$omp section
      do k=1, 2 
        call GET_COMMAND(COMMAND, LENGTH, STATUS)
        if ( (TRIM(COMMAND) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
             (STATUS .ne. 0) )                        &
        then
          error stop 64
        endif

        call GET_ENVIRONMENT_VARIABLE('XLSMPOPTS', VALUE, LENGTH, STATUS, .true.)
        if (TRIM(VALUE) .ne. 'SCHEDULE=DYNAMIC:PARTHDS=4') ERROR STOP 74

      end do

    !$omp section
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


        call GET_ENVIRONMENT_VARIABLE('XLSMPOPTS', VALUE, LENGTH, STATUS, .true.)
        if (TRIM(VALUE) .ne. 'SCHEDULE=DYNAMIC:PARTHDS=4') ERROR STOP 75

        END DO

    !$omp section

        NAME = 'CmdLine    '
        TRIM_NAME = .true.

      do k=1, 2 
        call GET_ENVIRONMENT_VARIABLE(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
        if ( (TRIM(VALUE) .ne. TRIM(CmdLine))  .or. &
             (LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
             (STATUS .ne. 0))                       &
        then
          error stop 66
        endif


        call GET_ENVIRONMENT_VARIABLE('XLSMPOPTS', VALUE, LENGTH, STATUS, .true.)
        if (TRIM(VALUE) .ne. 'SCHEDULE=DYNAMIC:PARTHDS=4') ERROR STOP 76

      end do  
 
     !$OMP END PARALLEL SECTIONS



      END 
 
      INCLUDE 'cmdline.include'


