!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-06-25
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : Use of PURE functions in the forall body
!*  ADAPTED FROM               : hpf_forall/construct/fxfc016.scenario
!*
!*  DESCRIPTION
!*
!*    Test PURE functions in the forall_body of the FORALL construct.
!*    Also test FORALL in a PURE function.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

MODULE mod

   TYPE dt
      SEQUENCE
      INTEGER i
      REAL    r
      INTEGER j
   END TYPE dt

END MODULE mod

PROGRAM fxfc016

   USE mod

   INTEGER CASENUM,in1,in2,in3

   INTEGER i1(20,20)

   INTEGER i2(-20:20)

   TYPE (dt) dt3(100),dt3b(50)

   INTEGER i4a(20,20),i4b(20,10,20)

   INTEGER i5a(-20:20),i5b(-20:20,10)

   INTERFACE
      PURE INTEGER FUNCTION func1( i,j )
         INTEGER, INTENT(IN) :: i
         INTEGER, INTENT(IN) :: j
      END FUNCTION func1

      PURE INTEGER FUNCTION func2( array )
         INTEGER, INTENT(IN), DIMENSION(:) :: array
      END FUNCTION

      PURE FUNCTION func3( array, i )
         TYPE dt
            SEQUENCE
            INTEGER i
            REAL    r
            INTEGER j
         END TYPE dt
         TYPE (dt),INTENT(IN) :: array(:)
         INTEGER,INTENT(IN) :: i
         TYPE (dt) :: func3
      END FUNCTION

      PURE FUNCTION func4( value, n )
         INTEGER,INTENT(IN) :: value,n
         INTEGER :: func4(n)
      END FUNCTION

      PURE FUNCTION func5( array )
         INTEGER, INTENT(IN) :: array(:)
         INTEGER func5(size(array))
      END FUNCTION

   END INTERFACE

!-------------------------------------
! Test PURE scalar function
!-------------------------------------

   CASENUM = 1
   PRINT *,"CASENUM=",CASENUM

   i1 = 0
   FORALL ( integer:: in1 = 1:20, in2 = 1:20 )
      i1(in1,in2) = func1(in1,in2)
   END FORALL
   PRINT *,i1

!---

   CASENUM = 2
   PRINT *,"CASENUM=",CASENUM

   i2 = 0
   forall ( integer(2)::in1 = -20:20 ) i2(in1) = in1
   FORALL ( integer(4)::in1 = -20:19:2 )
      i2(in1) = func2( i2(in1+1::2) )
   END FORALL
   PRINT *,i2

!---

   CASENUM = 3
   PRINT *,"CASENUM=",CASENUM

   dt3  = dt( 0, 0.0, 0 )
   dt3b = dt( 0, 0.0, 0 )
   forall ( integer::in1 = 1:100 ) dt3(in1) = dt(101-in1,in1+0.5,in1)
   FORALL ( integer(4)::in1 = 1:50:2 )
      dt3b(in1) = func3( dt3(in1::2),in1+1 )
      dt3b(in1+1) = func3( dt3(in1::2),in1 )
   END FORALL
   PRINT *,dt3b

!-------------------------------------
! Test PURE array function
!-------------------------------------

   CASENUM = 4
   PRINT *,"CASENUM=",CASENUM

   i4a = 0
   i4b = 0

   forall ( integer(2)::in1=1:20,in2=1:20 ) i4a(in1,in2) = in1*20 + in2
   FORALL ( integer(1)::in1 = 1:20,in2 = 1:20 )
      i4b(in1,::2,in2) = func4( i4a(in1,in2),5)
      i4b(in1,2::2,in2) = func4( i4a(in2,in1),5)
   END FORALL
   PRINT *,i4b

!---

   CASENUM = 5
   PRINT *,"CASENUM=",CASENUM

   i5a = 0
   i5b = 0
   forall ( integer(2)::in1 = -20:20 ) i5a(in1) = in1*20 + in1

   FORALL ( integer(4)::in1 = -20:-1:2 )
      i5b(in1,:) = func5(i5a(in1:in1+9))+in1
      i5b(in1+1,:) = func5(i5a(in1:in1+9))+in1
   END FORALL

   PRINT *,i5b

END

!--------------------------------
! Simple pure function which
! accepts two integers and
! adds them
!--------------------------------

PURE INTEGER FUNCTION func1( i,j )
   USE mod
   INTEGER, INTENT(IN) :: i
   INTEGER, INTENT(IN) :: j

   func1 = i + j
END FUNCTION func1

!--------------------------------
! Simple pure function which
! accepts a 1d array and sums it.
!--------------------------------

PURE INTEGER FUNCTION func2( array )
   USE mod
   INTEGER, INTENT(IN), DIMENSION(:) :: array
   INTEGER i

   func2 = 0
   DO i = 1,ubound(array,1)
      func2 = func2 + array(i)
   END DO
END FUNCTION

!--------------------------------
! Pure function which accepts a
! dt array and scalar i returning
! the first element whose component
! i has value i.
!--------------------------------

PURE FUNCTION func3( array, i )
   USE mod
   TYPE (dt),INTENT(IN) :: array(:)
   INTEGER,INTENT(IN) :: i
   TYPE (dt) :: func3
   INTEGER j,k

   func3 = dt( 0, 0.0, 0 )
   DO j = 1,ubound(array,1)
      IF ( array(j)%i .EQ. i ) THEN
        func3 = array(j)
      END IF
   END DO

END FUNCTION

!--------------------------------
! Pure function which accepts a
! scalar and a size and returns
! an array of that size containing
! that scalar + increments.
!--------------------------------

PURE FUNCTION func4( value, n )
   USE mod
   INTEGER,INTENT(IN) :: value,n
   INTEGER :: func4(n)
   INTEGER I

   func4 = (/(i,i=value,value+n-1)/)
END FUNCTION

!--------------------------------
! Pure function which accepts an
! array and returns the reverse
! of that array.
!--------------------------------

PURE FUNCTION func5( array )
   USE mod
   INTEGER, INTENT(IN) :: array(:)
   INTEGER func5(size(array))
   INTEGER in1

   FORALL ( integer(4)::in1 = 1:ubound(array,1) )
      func5(in1) = array( ubound(array,1) - in1 + 1 )
   END FORALL
END FUNCTION



