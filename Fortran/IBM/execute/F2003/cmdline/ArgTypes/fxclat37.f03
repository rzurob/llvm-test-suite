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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing elements of
!*                             : array of derived types with pointer attrubute from different modules
!*                             : as arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module m1

        type STR
          character(2049)  :: C
        end type STR

      end module m1

      module m2

        type  INT
          integer  :: C
        end type INT

      end module m2

      module m3

        type LOGIC
          logical  :: C
        end type LOGIC

      end module m3


      PROGRAM fxclat37


      use m2
      use m3
      use m1



      IMPLICIT NONE

      type(STR),    pointer  ::  PSTR(:)
      type(INT),    pointer  ::  PINT(:)
      type(LOGIC),  pointer  ::  PLOGIC(:)


      character(2049)              :: CmdLine = 'fxclat37 1'
      integer                      :: CmdCount, i
      character(2047)              :: Argument



      allocate (PSTR(3),     &
                PINT(4),     &
                PLOGIC(1))


!     PSTR(1)    => COMMAND
!     PINT(1)    => LENGTH
!     PINT(2)    => STATUS
!     PINT(3)    => NUMBER
!     PSTR(2)    => VALUE
!     PSTR(3)    => NAME
!     PLOGIC(1)  => TRIM_NAME
!     PINT(4)    => ARGCOUNT


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 1 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(PSTR(1)%C, PINT(1)%C, PINT(2)%C)

      if ( (TRIM(PSTR(1)%C) .ne. TRIM(CmdLine))   .or. &
           (PINT(1)%C .lt. LEN(TRIM(CmdLine)))    .or. &
           (PINT(1)%C .gt. LEN(CmdLine))          .or. &
           (PINT(2)%C .gt. 0) )                        &
      then
        error stop 64
      endif

      DO i  = 0, CmdCount

        PINT(3)%C = i
        call GET_COMMAND_ARGUMENT(PINT(3)%C, PSTR(2)%C, PINT(1)%C, PINT(2)%C)
        call MyGetArg(CmdLine, PINT(3)%C, Argument)
        if ( (TRIM(PSTR(2)%C) .ne. TRIM(Argument))      .or. &
             (PINT(1)%C      .lt. LEN(TRIM(Argument)))  .or. &
             (PINT(1)%C      .gt. LEN(Argument))        .or. &
             (PINT(2)%C      .gt. 0) )                       &
        then
          error stop 65
        endif

      END DO

      PSTR(3)%C = 'CmdLine     '
      PLOGIC(1)%C = .true.
      call GET_ENVIRONMENT_VARIABLE(PSTR(3)%C, PSTR(2)%C, PINT(1)%C, PINT(2)%C, PLOGIC(1)%C)
      if ( (TRIM(PSTR(2)%C) .ne. TRIM(CmdLine))  .or. &
           (PINT(1)%C .lt. LEN(TRIM(CmdLine)))   .or. &
           (PINT(1)%C .gt. LEN(CmdLine))         .or. &
           (PINT(2)%C .gt. 0))                        &
      then
        error stop 66
      endif


      deallocate (PSTR,     &
                  PINT,     &
                  PLOGIC)

      END

      INCLUDE 'cmdline.include'




