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
!*  DESCRIPTION                : Call command line intrinsic routines within an elemental function
!*                             : which is invoked through nested loop statements
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM fxcllf23

      IMPLICIT NONE


      character(513)   :: NAME(3)
      logical          :: TRIM_NAME(3)
      character(2049)  :: CmdLine(3)
      integer          :: i


      DATA (CmdLine(i), i = 1, 3)    /3*'fxcllf23 1 a 2'/
      DATA (NAME(i), i = 1, 3)       /3*'CmdLine   '/
      DATA (TRIM_NAME(i), i = 1, 3)  /3*.true./


      character(2049)  :: COMMAND(3)
      integer          :: LENGTH(3)
      integer          :: STATUS(3)
      integer          :: NUMBER(3)
      character(2047)  :: VALUE(3)
      integer          :: ARGCOUNT(3)


      DATA (COMMAND(i), i = 1, 3)    /3* '????? '/
      DATA (LENGTH(i), i = 1, 3)     /3* 1111 /
      DATA (STATUS(i), i = 1, 3)     / 3*1111 /
      DATA (NUMBER(i), i = 1, 3)     /3*2222/
      DATA (VALUE(i), i = 1, 3)      / 3* '!!!!'/
      DATA (ARGCOUNT(i), i = 1, 3)   /3* 0 /



      integer              :: CmdCount = 3
      character(2047)      :: Argument
      INTEGER              :: k, l, m, n
      LOGICAL              :: RESULT(3)


      INTERFACE

        ELEMENTAL FUNCTION ELEMENTAL_FUN(    &
                        NAME,      &
                        TRIM_NAME, &
                        CmdLine,   &
                        COMMAND,   &
                        LENGTH,    &
                        STATUS,    &
                        NUMBER,    &
                        VALUE,     &
                        ARGCOUNT   )
          LOGICAL ELEMENTAL_FUN
          character(513), INTENT(IN)   :: NAME
          logical, INTENT(IN)          :: TRIM_NAME
          character(2049), INTENT(IN)  :: CmdLine
          character(2049), INTENT(IN)  :: COMMAND
          integer, INTENT(IN)          :: LENGTH
          integer, INTENT(IN)          :: STATUS
          integer, INTENT(IN)          :: NUMBER
          character(2047), INTENT(IN)  :: VALUE
          integer, INTENT(IN)          :: ARGCOUNT
        END FUNCTION

      END INTERFACE




      DO k =1, 2
      DO l =1, 2
      DO m =1, 2
      DO n =1, 2

           RESULT =  ELEMENTAL_FUN(   &
                        NAME,         &
                        TRIM_NAME,    &
                        CmdLine,      &
                        COMMAND,      &
                        LENGTH,       &
                        STATUS,       &
                        NUMBER,       &
                        VALUE,        &
                        ARGCOUNT      &
                )
           IF ( ANY(RESULT )) ERROR STOP 67

      END DO
      END DO
      END DO
      END DO

      call GET_COMMAND(COMMAND(1), LENGTH(1), STATUS(1))
      if ( (TRIM(COMMAND(1)) .ne. TRIM(CmdLine(1)))  .or. &
           (LENGTH(1) .ne. LEN(TRIM(CmdLine(1))))    .or. &
           (STATUS(1) .ne. 0) )                        &
      then
        error stop 64
      endif


      DO i  = 0, CmdCount

        NUMBER = i
        call GET_COMMAND_ARGUMENT(NUMBER(2), VALUE(2), LENGTH(2), STATUS(2))
        call MyGetArg(CmdLine(2), NUMBER(2), Argument)

        if ( (TRIM(VALUE(2)) .ne. TRIM(Argument))       .or. &
             (LENGTH(2)      .ne. LEN(TRIM(Argument)))  .or. &
             (STATUS(2)      .ne. 0) )                       &
        then
          error stop 65
        endif

      END DO



      call GET_ENVIRONMENT_VARIABLE(NAME(3), VALUE(3), LENGTH(3), STATUS(3), TRIM_NAME(3))
      if ( (TRIM(VALUE(3)) .ne. TRIM(CmdLine(3)))  .or. &
            (LENGTH(3) .ne. LEN(TRIM(CmdLine(3))))  .or. &
            (STATUS(3) .ne. 0))                       &
      then
         error stop 66
      endif

      END


      ELEMENTAL FUNCTION ELEMENTAL_FUN(NAME, TRIM_NAME, CmdLine,  &
                                    COMMAND, LENGTH, STATUS, NUMBER, VALUE, ARGCOUNT)

      LOGICAL ELEMENTAL_FUN

      character(513), INTENT(IN)   :: NAME
      logical, INTENT(IN)          :: TRIM_NAME
      character(2049), INTENT(IN)  :: CmdLine

      character(2049), INTENT(IN)  :: COMMAND
      integer, INTENT(IN)          :: LENGTH
      integer, INTENT(IN)          :: STATUS
      integer, INTENT(IN)          :: NUMBER
      character(2047), INTENT(IN)  :: VALUE
      integer, INTENT(IN)          :: ARGCOUNT

      integer, automatic           :: CmdCount


      ELEMENTAL_FUN = .false.

      CmdCount = COMMAND_ARGUMENT_COUNT() ! only this function is pure
      if ( CmdCount .ne. 3 ) &
      then
        ELEMENTAL_FUN = .TRUE.
        !all zzrc(63)
      endif


      END FUNCTION



      INCLUDE 'cmdline.include'

