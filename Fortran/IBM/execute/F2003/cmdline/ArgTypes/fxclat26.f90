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
!*  DESCRIPTION                : Tests command line intrinsic routines by passing named parameter
!*                             : and components from different STRUCTURES (Union/Map)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


      module modtype

         STRUCTURE /C/
           character(2049)      :: COMMAND
         END STRUCTURE

         type L
           integer              :: LENGTH
         end type L

         STRUCTURE /S/
           integer              :: STATUS
         END STRUCTURE

         type Nu
           integer              :: NUMBER
         end type Nu

         STRUCTURE /V/
           UNION
             MAP
               character(2047)  :: VALUE
             END MAP
           END UNION

           UNION
             MAP
               REAL             :: NoUse(200)
             END MAP
           END UNION
         END STRUCTURE


         type A
           integer              :: ARGCOUNT
         end type A

         RECORD /C/  C
         type(L)     L
         RECORD /S/  S
         type(Nu)    Nu
         RECORD /V/  V
         type(A)     A

         character(513),parameter      :: NAME= 'CmdLine   '
         logical, parameter            :: TRIM_NAME =.true.

      end module modtype


      PROGRAM fxclat26

      use modtype

      IMPLICIT NONE


      character(2049)              :: CmdLine = 'fxclat26 1 a 2'
      integer                      :: CmdCount, i
      character(2047)              :: Argument
      integer LENGTH
      integer STATUS


      CmdCount = COMMAND_ARGUMENT_COUNT()
      if ( CmdCount .ne. 3 ) &
      then
        error stop 63
      endif

      call GET_COMMAND(COMMAND=C%COMMAND(33:532), LENGTH=L%LENGTH, STATUS=S%STATUS)
      call GET_COMMAND()
      if ( (TRIM(C%COMMAND(33:532)) .ne. TRIM(CmdLine))  .or. &
           (L%LENGTH .ne. LEN(TRIM(CmdLine)))            .or. &
           (S%STATUS .ne. 0) )                                &
      then
        error stop 64
      endif

      call GET_COMMAND(COMMAND=C%COMMAND(1:101), LENGTH=L%LENGTH)
      if ( (TRIM(C%COMMAND(1:101)) .ne. TRIM(CmdLine))  .or. &
           (L%LENGTH .ne. LEN(TRIM(CmdLine))))               &
      then
        error stop 65
      endif

      call GET_COMMAND(COMMAND=C%COMMAND(1001:2049))
      if ( TRIM(C%COMMAND(1001:2049)) .ne. TRIM(CmdLine))  &
      then
        error stop 66
      endif


      DO i  = 0, CmdCount

        Nu%NUMBER = i
        call MyGetArg(CmdLine, Nu%NUMBER, Argument)

        call GET_COMMAND_ARGUMENT(NUMBER=Nu%NUMBER, VALUE=V%VALUE(1023:2046), LENGTH=L%LENGTH,STATUS=S%STATUS)
        call GET_COMMAND_ARGUMENT(NUMBER=Nu%NUMBER)

        if ( (TRIM(V%VALUE(1023:2046)) .ne. TRIM(Argument))     .or. &
             (L%LENGTH      .ne. LEN(TRIM(Argument)))           .or. &
             (S%STATUS      .ne. 0) )                                &
        then
          error stop 67
        endif

        call GET_COMMAND_ARGUMENT(NUMBER=Nu%NUMBER, VALUE=V%VALUE(513:688), LENGTH=L%LENGTH)
        if ( (TRIM(V%VALUE(513:688)) .ne. TRIM(Argument)) .or. &
             (L%LENGTH      .ne. LEN(TRIM(Argument))))         &
        then
          error stop 68
        endif

        call GET_COMMAND_ARGUMENT(Nu%NUMBER, VALUE =V%VALUE(11:511), STATUS=S%STATUS)
        if ( (TRIM(V%VALUE(11:511)) .ne. TRIM(Argument))  .or. &
             (S%STATUS      .ne. 0) )                          &
        then
          error stop 69
        endif

      END DO

      call GET_ENVIRONMENT_VARIABLE(NAME, VALUE=V%VALUE(1013:2039), LENGTH=L%LENGTH, STATUS=S%STATUS, TRIM_NAME=TRIM_NAME)
      if ( (TRIM(V%VALUE(1013:2039)) .ne. TRIM(CmdLine))  .or. &
           (L%LENGTH .ne. LEN(TRIM(CmdLine)))             .or. &
           (S%STATUS .ne. 0))                                  &
      then
        error stop 70
      endif

      call GET_ENVIRONMENT_VARIABLE(NAME, LENGTH=LENGTH, STATUS= STATUS, TRIM_NAME=TRIM_NAME)
      if ( (LENGTH .ne.L%LENGTH)  .or. &
           (STATUS.ne. S%STATUS))      &
      then
        error stop 71
      endif


      END


      INCLUDE 'cmdline.include'






