
   implicit none

   integer CASENUM
   integer in1,in2

   !--------------------------------
   ! Variable Declarations
   !--------------------------------

   INTEGER l1a(100)

   INTEGER i1a(100)

   INTEGER i1b(100,10)

   REAL r1a(100,10)

   REAL r1b(100,10)

   REAL r1c(100,10)

   !--------------------------------
   ! End of Variable Declarations
   !--------------------------------

   !--------------------------------
   ! Interface Block
   !--------------------------------

   INTERFACE

      PURE LOGICAL FUNCTION func1( l )
         INTEGER,INTENT(IN) :: l
      END FUNCTION

   END INTERFACE

   !--------------------------------
   ! End of Interface Block
   !--------------------------------

   l1a = (/ (in1,in1=1,100) /)

   CASENUM = 7
   PRINT *,CASENUM

   i1a = 0
   i1b = 0
   r1a = 0.0
   r1b(:,::2) = 1.0
   r1b(:,2::2) = 2.0
   r1c = 0.0

   DO CONCURRENT( in1 = 1:100, func1(l1a(in1)) )
      i1a(in1) = in1
   END DO

   DO CONCURRENT ( in1 = 1:100, func1(l1a(in1)) )  !<-- if replaced with FORALL, then this will get the expected result
      FORALL ( in2 = 1:10, func1(l1a(in2)) )
         i1b(in1,in2) = in1 + in2
      END FORALL

      forall ( in2=1:10:2 ) r1a(in1,in2) = real(in1+in2)
      WHERE ( r1b(in1,:) > 1.0 ) r1b(in1,:) = 99
      WHERE ( r1b(in1,:) > 1.0 )
         r1c(in1,:) = 99
      ELSEWHERE
         r1c(in1,:) = -99
      END WHERE
   END DO
   print *,r1b

END

!----------------------------------------------------

PURE LOGICAL FUNCTION func1( l )
   INTEGER,INTENT(IN) :: l

   if ( MODULO(l,3) == 0 ) THEN
      func1 = .TRUE.
   else
      func1 = .FALSE.
   end if

END FUNCTION
