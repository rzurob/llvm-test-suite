! *********************************************************************
! %START
! %MAIN: YES
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclpl19 nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn\&"
! %COMPOPTS:  -qfree=f90  -qnosave
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclpl19
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclpl19.f
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
!*  DESCRIPTION                :  Call command line intrinsic routines within an external sub  
!*                             :  which is invoked through a nested parallel do construct
!*            
!*                     
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxclp19
      
      IMPLICIT NONE

      integer i, j, k



   !$OMP PARALLEL DO  DEFAULT(PRIVATE)
      do i = 1, 2
   !$OMP PARALLEL DO 
         do j = 1, 2
   !$OMP PARALLEL DO 
            do k =1, 2
               call sub
           end do
   !$OMP END PARALLEL DO 
        end do
   !$OMP END PARALLEL DO 
      end do
   !$OMP END PARALLEL DO 


      do i = 1, 1
          call sub
      end do


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
     

        CmdLine = 'fxclpl19 nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn\\&'
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



