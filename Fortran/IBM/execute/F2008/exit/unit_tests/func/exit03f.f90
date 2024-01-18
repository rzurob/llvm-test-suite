!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : exit03f.f
!*
!* PROGRAMMER                 : David Nichols
!* DATE                       : March 8, 2011
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : EXIT Statement
!*
!* DRIVER STANZA              : xlf2008
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              EXIT statement from named constructs
!*                              with no construct_name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program exit03f
      class(*), pointer :: c
      integer :: i,j,k
      allocate (c, source=i)
      i = 0

      DO j=1, 10
        a1 : ASSOCIATE ( x => j )
          if (x .EQ. 3) then
            EXIT 
          else
            i = i + 1
          endif
        end ASSOCIATE a1
      ENDDO

      if (i .NE. 2) then; ERROR STOP 1; else; i = 0; endif

      DO j=1,10
        b1 : BLOCK
          if (j .EQ. 4) then
            EXIT 
          else
            i = i + 1
          endif
        end BLOCK b1
      ENDDO

      if (i .NE. 3) then; ERROR STOP 2; else; i = 0; endif

      DO j=1,10
        d1 : DO k=1, 10
          if (j .EQ. 5 .AND. k .EQ. 1) then
            EXIT 
          elseif (k .EQ. 1) then
            i = i + 1
          endif
        ENDDO d1
        if (j .EQ. 5) then
          EXIT 
        endif
      ENDDO

      if (i .NE. 4) then; ERROR STOP 3; else; i = 0; endif

      DO j=1,10
        i1 : IF (i .EQ. (j-1)) then
          if (j .EQ. 6) then
            EXIT 
          else
            i = i + 1
          endif
        ENDIF i1
      ENDDO

      if (i .NE. 5) then; ERROR STOP 4; else; i = 0; endif

      DO j=1,10
        sc1 : SELECT CASE (j)
        CASE (:5) sc1
          i = i + 1
          EXIT sc1
        CASE (6) sc1
          if (j .NE. 6) then
            EXIT 
          else 
            i = i + 1
            EXIT sc1
          endif
        CASE (7:) sc1
          EXIT
        END SELECT sc1
      ENDDO

      if (i .NE. 6) then; ERROR STOP 5; else; i = 0; endif

      DO j=1,10
        st1 : SELECT TYPE (c)
        TYPE IS (real) st1
          ERROR STOP 8
        TYPE IS (integer) st1
          if (j .EQ. 8) then
            EXIT 
          else
            i = i + 1
          endif
        CLASS DEFAULT st1
          ERROR STOP 9
        END SELECT st1
      ENDDO

      if (i .NE. 7) then; ERROR STOP 6; else; i = 0; endif

      end
