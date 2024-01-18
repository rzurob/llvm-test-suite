!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : exit01f.f
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
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program exit01f
      class(*), pointer :: c
      integer :: i,j
      allocate (c, source=i)
      i = 0

      a1 : ASSOCIATE ( x => 1 )
        EXIT a1
        i = x
      end ASSOCIATE a1

      if (i .NE. 0) then; ERROR STOP 1; endif

      b1 : BLOCK
        EXIT b1
        i = 1
      end BLOCK b1

      if (i .NE. 0) then; ERROR STOP 2; endif

      d1 : DO j=1, 10
        EXIT d1
        i = j
      ENDDO d1

      if (i .NE. 0) then; ERROR STOP 3; endif

      i = 1
      i1 : IF (i .EQ. 1) then
        i = 0
        EXIT i1
        i = 1
      ENDIF i1

      if (i .NE. 0) then; ERROR STOP 4; endif

      sc1 : SELECT CASE (i)
      CASE (:-1) sc1
        ERROR STOP 5
      CASE (0) sc1
        EXIT sc1
        i = 1
      CASE (1:) sc1
        ERROR STOP 6
      END SELECT sc1

      if (i .NE. 0) then; ERROR STOP 7; endif

      st1 : SELECT TYPE (c)
      TYPE IS (real) st1
        ERROR STOP 8
      TYPE IS (integer) st1
        EXIT st1
        i = 1
      CLASS DEFAULT st1
        ERROR STOP 9
      END SELECT st1

      if (i .NE. 0) then; ERROR STOP 10; endif

      end
