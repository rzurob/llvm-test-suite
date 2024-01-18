!* =================================================================== &
!*
!* DATE                       : March 8, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : EXIT Statement
!*
!* DESCRIPTION                : Testing proper diagnostics of
!*                              EXIT statement without construct_name
!*                              and without containing DO
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program exit02d
      class(*), pointer :: c
      integer :: i,j
      allocate (c, source=i)
      i = 0

      a1 : ASSOCIATE ( x => 1 )
        EXIT a2
        i = x
      end ASSOCIATE a1

      if (i .NE. 0) then; STOP 1; endif

      b1 : BLOCK
        EXIT b2
        i = 1
      end BLOCK b1

      if (i .NE. 0) then; STOP 2; endif

      d1 : DO j=1, 10
        EXIT d2
        i = j
      ENDDO d1

      if (i .NE. 0) then; STOP 3; endif

      i = 1
      i1 : IF (i .EQ. 1) then
        i = 0
        EXIT i2
        i = 1
      ENDIF i1

      if (i .NE. 0) then; STOP 4; endif

      sc1 : SELECT CASE (i)
      CASE (:-1) sc1
        STOP 5
      CASE (0) sc1
        EXIT sc2
        i = 1
      CASE (1:) sc1
        STOP 6
      END SELECT sc1

      if (i .NE. 0) then; STOP 7; endif

      st1 : SELECT TYPE (c)
      TYPE IS (real) st1
        STOP 8
      TYPE IS (integer) st1
        EXIT st2
        i = 1
      CLASS DEFAULT st1
        STOP 9
      END SELECT st1

      if (i .NE. 0) then; STOP 10; endif

      end
