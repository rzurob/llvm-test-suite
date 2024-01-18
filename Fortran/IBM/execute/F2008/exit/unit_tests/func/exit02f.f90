!* =================================================================== &
!*
!* DATE                       : March 8, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : EXIT Statement
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              EXIT in nested constructs
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program exit02f
      class(*), pointer :: c
      integer :: i,j
      allocate (c, source=i)
      i = 0

      a1 : ASSOCIATE ( x => 1 )
        a2 : ASSOCIATE ( y => 1 )
          a3 : ASSOCIATE ( z => 1 )
            EXIT a3
            i = z
          end ASSOCIATE a3
          EXIT a2
          i = y
        end ASSOCIATE a2
        EXIT a1
        i = x
      end ASSOCIATE a1

      if (i .NE. 0) then; ERROR STOP 1; endif

      a4 : ASSOCIATE ( x => 1 )
        d1 : DO j=1, 10
          i1 : IF (i .EQ. 0) then
            sc1 : SELECT CASE (i)
            CASE (0) sc1
              st1 : SELECT TYPE (c)
              TYPE IS (real) st1
                ERROR STOP 2
              TYPE IS (integer) st1
                b1 : BLOCK
                  EXIT a4
                  i = j
                end block b1
              CLASS DEFAULT st1
                ERROR STOP 3
              END SELECT st1
            END SELECT sc1
          endif i1
        enddo d1
      end associate a4

      if (i .NE. 0) then; ERROR STOP 4; endif

      b2 : BLOCK
        d2 : DO j=1, 10
          if (j .EQ. 5) then
            EXIT b2
          endif
          i = j
        ENDDO d2
        i = 1
      end BLOCK b2

      if (i .NE. 4) then; ERROR STOP 5; endif

      i2 : IF (i .EQ. 1) then
        ERROR STOP 6
      ELSEIF (i .EQ. 2) then i2
        ERROR STOP 7
      ELSEIF (i .EQ. 4) then i2
        i3 : IF (i .EQ. 5) then
          ERROR STOP 8
        ELSEIF (i .EQ. 4) then i3
          i = 7
          i4 : IF (i .EQ. 0) then
            ERROR STOP 9
          ELSEIF (i .EQ. 4) then i4
            ERROR STOP 10
          ELSE i4
            EXIT i3
          ENDIF i4
          ERROR STOP 11
        ENDIF i3
        i = i + 1
      ELSE i2
        ERROR STOP 12
      ENDIF i2

      if (i .NE. 8) then; ERROR STOP 13; endif

      i = 9

      d3 : DO j=-1,1
        i5 : if (i .NE. (10+j)) then
          ERROR STOP 14
        ELSE i5
          EXIT i5
        ENDIF i5
        sc2 : SELECT CASE (j)
        CASE (:-1) sc2
          a10 : ASSOCIATE ( a => j )
            i = 10
            EXIT sc2
            i = 20
          END ASSOCIATE a10
        CASE (0) sc2
          st2 : SELECT TYPE (c)
          TYPE IS (real) st2
            ERROR STOP  15
          TYPE IS (integer) st2
            i = 11
            EXIT st2
            i = 21
          CLASS DEFAULT st2
            ERROR STOP  16
          END SELECT st2
          EXIT sc2
          i = 1
        CASE (1:) sc2
          b6 : BLOCK
            i = 12
            EXIT sc2
            i = 22
          END BLOCK b6
        END SELECT sc2
      ENDDO d3

      if (i .NE. 12) then; ERROR STOP 17; endif

      end
