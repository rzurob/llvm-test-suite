!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_f07.f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 2012-06-25
!*  ORIGIN                     : 
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : Verify that the compiler can declare the index
!*                               for the innermost loop (Test 7)
!*  ADAPTED FROM               : from hpf_forall/construct/fxfc015b.scenario
!*
!*  DESCRIPTION
!*  
!*    Test a PURE function as the scalar-mask-expr in a forall-construct.  It 
!*    also contains coverage with the WHERE construct.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM fxfc015b


   integer CASENUM

   !--------------------------------
   ! Variable Declarations
   !--------------------------------

   INTEGER i1a(-20:20)

   REAL r1a(-20:20,-5:5)

   REAL r1b(-20:20,-5:5) 

   REAL r1c(-20:20,20)
  
   REAL r1d(-20:20,20)

   !--------------------------------
   ! End of Variable Declarations
   !--------------------------------

   !--------------------------------
   ! Interface Block
   !--------------------------------

   INTERFACE

      PURE LOGICAL FUNCTION func2( i )
         INTEGER,INTENT(IN) :: i
      END FUNCTION

   END INTERFACE

   !--------------------------------
   ! End of Interface Block
   !--------------------------------


   !-------------------------------
   !  Test forall assignment
   !-------------------------------
   CASENUM = 1
   PRINT *,CASENUM

   i1a = 0
   FORALL ( integer::in1 = -20:20 , func2(in1) )
      i1a(in1) = in1
   END FORALL

   print *,i1a

   !-------------------------------
   !  Test forall statement
   !-------------------------------
   CASENUM = 3
   PRINT *,CASENUM

   r1a = 0.0
   FORALL ( integer::in1 = -20:20, func2(in1) )
      forall ( in2 = -5:5, func2(in2) ) r1a(in1,in2) = in1 + in2
   END FORALL

   print *,r1a

   !-------------------------------
   !  Test forall construct
   !-------------------------------
   CASENUM = 4
   PRINT *,CASENUM

   r1b = 0.0
   FORALL ( integer::in1 = -20:20, func2(in1) )
      FORALL ( integer(4)::in2 = -5:5, func2(in2) ) 
         r1b(in1,in2) = in1 + in2
         r1b(in1,-in2) = in1 + in2
      END FORALL
   END FORALL
 
   print *,r1b

   !-------------------------------
   !  Test where statement
   !-------------------------------
   CASENUM = 5
   PRINT *,CASENUM

   r1c(:,::2) = 1.0
   r1c(:,2::2) = 2.0
   FORALL ( integer::in1 = -20:20, func2(in1) )
      WHERE ( r1c(in1,:) > 1.0 ) r1c(in1,:) = 99
   END FORALL
    
   print *,r1c

   !-------------------------------
   !  Test where construct
   !-------------------------------
   CASENUM = 6
   PRINT *,CASENUM

   r1d(:,::2) = 1.0
   r1d(:,2::2) = 1.0
   FORALL ( integer::in1 = -20:20, func2(in1) )
      WHERE ( r1d(in1,:) > 1.0 )
         r1d(in1,:) = 99
         r1d(in1,:) = r1d(in1,:) + 1
      END WHERE
   END FORALL

   print *,r1d
   

   !-------------------------------
   !  Test all
   !-------------------------------
   CASENUM = 7
   PRINT *,CASENUM

   i1a = 0
!  i1t = 0
   r1a = 0.0
   r1b = 0.0
   r1c(:,::2)  = 1.0
   r1c(:,2::2) = 2.0
   r1d(:,::2)  = 1.0
   r1d(:,2::2) = 2.0

   FORALL ( integer::in1 = -20:20, func2(in1) )
      i1a(in1) = in1
!     i1p(in1) = in1
      forall ( in2 = -5:5, func2(in2) ) r1a(in1,in2) = in1 + in2
      FORALL ( in2 = -5:5, func2(in2) )
         r1b(in1,in2) = in1 + in2
         r1b(in1,-in2) = in1 + in2
      END FORALL
      WHERE ( r1c(in1,:) > 1.0 ) r1c(in1,:) = 99 
      WHERE ( r1d(in1,:) > 1.0 )
         r1d(in1,:) = 99
         r1d(in1,:) = r1d(in1,:) + 1
      END WHERE
   END FORALL

   print *,i1a
!  print *,i1t
   print *,r1a
   print *,r1b
   print *,r1c
   print *,r1d

END

!----------------------------------------------------

PURE LOGICAL FUNCTION func2( i )
   INTEGER,INTENT(IN) ::i

   func2 = .FALSE.
   IF ( i > 0 ) func2 = .TRUE.
END FUNCTION

!----------------------------------------------------
