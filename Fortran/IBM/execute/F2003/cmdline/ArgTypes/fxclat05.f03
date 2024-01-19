! *********************************************************************
!*  ===================================================================
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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing components
!*                             : of record structure defined in common block and initialized in
!*                             : data block
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module modtype

        STRUCTURE /ST/
          character(2049)  :: COMMAND
          integer      	   :: LENGTH
          character(4099)  :: STR     ! take spaces
          integer          :: STATUS
          integer          :: NUMBER
          character(2047)  :: VALUE
          INTEGER          :: ARR(10) ! take spaces
          character(513)   :: NAME
          logical          :: TRIM_NAME
          integer          :: ARGCOUNT
        END STRUCTURE

      end module modtype


      PROGRAM fxclat05

      use modtype

      IMPLICIT NONE


      character(4099) 	STR
      INTEGER         	ARR(10)

      character(2049)              :: CmdLine = 'fxclat05 \\\/\\\/ \\\*'
      integer                      :: CmdCount, i
      character(2047)              :: Argument

      RECORD /ST/ cmd
      common /blk/cmd


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 2 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(cmd%COMMAND, cmd%LENGTH, cmd%STATUS)
      if ( (TRIM(cmd%COMMAND) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))    .or. &
           (cmd%STATUS .ne. 0) )                        &
      then
        error stop 64
      endif

      DO i  = 0, CmdCount

        cmd%NUMBER = i
        call GET_COMMAND_ARGUMENT(cmd%NUMBER, cmd%VALUE, cmd%LENGTH, cmd%STATUS)
        call MyGetArg(CmdLine, cmd%NUMBER, Argument)

        if ( (TRIM(cmd%VALUE) .ne. TRIM(Argument))       .or. &
             (cmd%LENGTH      .ne. LEN(TRIM(Argument)))  .or. &
             (cmd%STATUS      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE(cmd%NAME, cmd%VALUE, cmd%LENGTH, cmd%STATUS, cmd%TRIM_NAME)
      if ( (TRIM(cmd%VALUE) .ne. TRIM(CmdLine))  .or. &
           (cmd%LENGTH .ne. LEN(TRIM(CmdLine)))  .or. &
           (cmd%STATUS .ne. 0))                       &
      then
        error stop 66
      endif


      END

      INCLUDE 'cmdline.include'


      BLOCK DATA BLOCKDATA
         USE modtype
         RECORD /ST/ cmd
         common /blk/cmd

         DATA cmd%STR /'1234567890'/
         DATA cmd%ARR /10*1000/
         DATA cmd%NAME /'CmdLine     '/
         DATA cmd%TRIM_NAME  / .true./

      END BLOCK DATA


