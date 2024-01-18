! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: export CmdLine="fxclat13 --- iiiii \<\<\< \&"
! %COMPOPTS:  -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: $TR_SRC/cmdline.sh fxclat13
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxclat13.f
!*
!*  DATE                       : Sept 18, 2003
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing allocatable array element
!*                             : multiple levels of derived type as actual arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      module modtype

        type level3
          sequence
          integer, allocatable          :: STATUS
          integer, allocatable          :: NUMBER
          character(2047), allocatable  :: VALUE
          INTEGER, allocatable          :: ARR(:) ! take spaces
          character(513), allocatable   :: NAME
          logical, allocatable          :: TRIM_NAME
          integer, allocatable          :: ARGCOUNT
        end type

        type level2
          sequence
          integer, allocatable          :: LENGTH
          character(4099), allocatable  :: STR     ! take spaces
          type(level3), allocatable     :: l3
        end type

        type level1
          sequence
          character(2049), allocatable  :: COMMAND
          type(level2), allocatable     :: l2
        end type

      end module modtype


      PROGRAM fxclat13

      USE modtype
      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclat13 --- iiiii \\\<\\\<\\\< \\\&'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      type(level1), allocatable ::  cmd(:)

      allocate(cmd(3))

      allocate(cmd(2)%COMMAND)
      allocate(cmd(2)%l2)

      allocate(cmd(2)%l2%LENGTH)
      allocate(cmd(2)%l2%STR)
      allocate(cmd(2)%l2%l3)

      allocate(cmd(2)%l2%l3%STATUS)
      allocate(cmd(2)%l2%l3%NUMBER)
      allocate(cmd(2)%l2%l3%VALUE)
      allocate(cmd(2)%l2%l3%ARR(10))
      allocate(cmd(2)%l2%l3%NAME)
      allocate(cmd(2)%l2%l3%TRIM_NAME)
      allocate(cmd(2)%l2%l3%ARGCOUNT)



      cmd(2)%l2%STR (127:327)= '1234567890'
      cmd(2)%l2%l3%ARR = 1000
      cmd(2)%l2%l3%NAME = 'CmdLine     '
      cmd(2)%l2%l3%TRIM_NAME  =  .true.


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 4 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(cmd(2)%COMMAND, cmd(2)%l2%LENGTH, cmd(2)%l2%l3%STATUS)


      if ( (TRIM(cmd(2)%COMMAND) .ne. TRIM(CmdLine))     .or. &
           (cmd(2)%l2%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (cmd(2)%l2%l3%STATUS .ne. 0) )                     &
      then
        error stop 64
      endif

      DO i  = 0, CmdCount

        cmd(2)%l2%l3%NUMBER = i
        call GET_COMMAND_ARGUMENT(cmd(2)%l2%l3%NUMBER, cmd(2)%l2%l3%VALUE, cmd(2)%l2%LENGTH, cmd(2)%l2%l3%STATUS)
        call MyGetArg(CmdLine, cmd(2)%l2%l3%NUMBER, Argument)

        if ( (TRIM(cmd(2)%l2%l3%VALUE) .ne. TRIM(Argument))       .or. &
             (cmd(2)%l2%LENGTH         .ne. LEN(TRIM(Argument)))  .or. &
             (cmd(2)%l2%l3%STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE(cmd(2)%l2%l3%NAME, cmd(2)%l2%l3%VALUE, cmd(2)%l2%LENGTH, cmd(2)%l2%l3%STATUS, cmd(2)%l2%l3%TRIM_NAME)
      if ( (TRIM(cmd(2)%l2%l3%VALUE) .ne. TRIM(CmdLine))  .or. &
           (cmd(2)%l2%LENGTH .ne. LEN(TRIM(CmdLine)))     .or. &
           (cmd(2)%l2%l3%STATUS .ne. 0))                       &
      then
        error stop 66
      endif


   !  deallocate(cmd(2)%l2%l3%STATUS)
   !  deallocate(cmd(2)%l2%l3%NUMBER)
   !  deallocate(cmd(2)%l2%l3%VALUE)
   !  deallocate(cmd(2)%l2%l3%ARR)
   !  deallocate(cmd(2)%l2%l3%NAME)
   !  deallocate(cmd(2)%l2%l3%TRIM_NAME)
   !  deallocate(cmd(2)%l2%l3%ARGCOUNT)

   !  deallocate(cmd(2)%l2%LENGTH)
   !  deallocate(cmd(2)%l2%STR)
   !  deallocate(cmd(2)%l2%l3)  !ICE

   !  deallocate(cmd(2)%COMMAND)
   !  deallocate(cmd(2)%l2)   !ICE

      deallocate(cmd)

      END

      INCLUDE 'cmdline.include'



