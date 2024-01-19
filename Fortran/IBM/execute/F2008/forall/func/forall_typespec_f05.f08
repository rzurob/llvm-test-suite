!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2012-06-25
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               : hpf_forall/construct/fxfc006.scenario
!*
!*  DESCRIPTION
!*
!*    Test multiple nesting of forall constructs, nesting the forall constructs
!*    with header dependencies, headers with different type specifiers.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM fxfc006

   integer CASENUM

   !--------------------------------
   ! Variable Declarations
   !--------------------------------

   INTEGER i1a(20,20)
   INTEGER i2a(20,20),i2b(20,20)
   INTEGER i3a(20,20)
   INTEGER i4a(20,20)
   INTEGER i5a(20,20),i5b(20,20)
   INTEGER i6a(20,20)
   INTEGER i7a(-10:10,-10:10)
   INTEGER i8a(0:9,1000:1019)
   INTEGER i9a(20,20)
   INTEGER i10a(20,20)

   !--------------------------------
   ! End of Variable Declarations
   !--------------------------------


   !-----------------------
   ! Simple forall construct
   !-----------------------
   CASENUM = 1
   PRINT *,CASENUM

   FORALL ( integer(1)::in1 = 1:20 )
      FORALL ( integer(1)::in2 = 1:20 )
         i1a(in1,in2) = in1 + in2
      END FORALL
   END FORALL

   print *,i1a

   !-----------------------
   ! Independent forall constructs
   !-----------------------
   CASENUM = 2
   PRINT *,CASENUM

   FORALL ( integer(2)::in1 = 1:20 )
      FORALL(integer(2)::in2=1:20)
         i2a(in1,in2) = in1
      END FORALL
      FORALL(integer(2)::in2=1:20)
         i2b(in1,in2) = in2
      END FORALL
   END FORALL

   print *,i2a
   print *,i2b

   !-----------------------
   ! Depdendent forall constructs
   !-----------------------
   CASENUM = 3
   PRINT *,CASENUM

   FORALL ( integer::in1 = 1:20 )
      FORALL(integer::in2=1:20)
         i3a(in1,in2) = in1
      END FORALL
      FORALL(integer::in2=1:20)
         i3a(in1,in2) = i3a(in1,in2) + in2
      END FORALL
   END FORALL

   print *,i3a

   !-----------------------
   ! forall-header has dependency
   !-----------------------
   CASENUM = 4
   PRINT *,CASENUM

   i4a = 0
   FORALL ( integer(1)::in1 = 1:20 )
      FORALL ( integer(2)::in2 = in1:20 )
         i4a(in1,in2) = in1 + in2
      END FORALL
   END FORALL

   print *,i4a

   !-----------------------
   ! forall-header depencies
   !-----------------------
   CASENUM = 5
   PRINT *,CASENUM

   i5a = 0
   i5b = 0
   FORALL ( integer(1)::in1 = 1:20 )
      FORALL( integer(2)::in2 = 1:20 )
         i5a(in1,in2) = in1
      END FORALL
      FORALL( integer(4)::in2 = 1:i5a(in1,1) )
         i5b(in1,in2) = in1 + in2
      END FORALL
   END FORALL

   print *,i5a
   print *,i5b

   !-----------------------
   ! Test forall construct scope
   !-----------------------
   CASENUM = 6
   PRINT *,CASENUM

   in2 = -99
   i6a = 0
   FORALL ( integer(8)::in1 = 1:20 )
      i6a(in1,1) = in2
      FORALL ( integer(8)::in2 = 2:19 )
         i6a(in1,in2) = in1 + in2
      END FORALL
      i6a(in1,20) = in2
   END FORALL

   print *,i6a

   !-----------------------
   ! Test non-default bounds
   !-----------------------
   CASENUM = 7
   PRINT *,CASENUM

   FORALL ( integer(1)::in1 = -10:10 )
      FORALL ( integer(1)::in2 = -10:10 )
         i7a(in1,in2) = in1 + in2
      END FORALL
   END FORALL

   print *,i7a

   !-----------------------
   ! Test non-default bounds
   !-----------------------
   CASENUM = 8
   PRINT *,CASENUM

   FORALL ( integer(8)::in1 = lbound(i8a,2):ubound(i8a,2) )
      FORALL ( integer(8)::in2 = lbound(i8a,1):ubound(i8a,1) )
         i8a(in2,in1) = in1 + in2
      END FORALL
   END FORALL

   print *,i8a

   !-----------------------
   ! Try reversing forall construct
   !-----------------------
   CASENUM = 9
   PRINT *,CASENUM

   FORALL ( integer(1)::in1 = 1:20 )
      FORALL ( integer(2)::in2 = 20:1:-1 )
         i9a(in1,in2) = in1 + in2
      END FORALL
   END FORALL

   print *,i9a

   !-----------------------
   ! forall construct with stride
   !-----------------------
   CASENUM = 10
   PRINT *,CASENUM

   i10a = 0
   FORALL ( integer(1)::in1 = 1:20 )
      FORALL ( integer(2)::in2 = 1:20:2 )
         i10a(in1,in2) = in1
      END FORALL
      FORALL ( integer(2)::in2 = 20:1:-2 )
         i10a(in1,in2) = in2
      END FORALL
   END FORALL

   print *,i10a

   !-----------------------
   ! Test Large FORALL
   !-----------------------
   CASENUM = 11
   PRINT *,CASENUM

   i1a = 0
   i2a = 0
   i2b = 0
   i3a = 0
   i4a = 0
   i5a = 0
   i5b = 0
   i6a = 0
   i9a = 0
   i10a = 0
   in2 = -99
   FORALL ( integer(1)::in1 = 1:20 )
      FORALL ( integer(2)::in2 = 1:20 )
         i1a(in1,in2) = in1 + in2
         i2a(in1,in2) = in1
         i2b(in1,in2) = in2
         i3a(in1,in2) = in1
         i3a(in1,in2) = i3a(in1,in2) + in2
      END FORALL
      FORALL ( integer(2)::in2 = in1:20 )
         i4a(in1,in2) = in1 + in2
      END FORALL
      i6a(in1,1) = in2
      FORALL ( integer(2)::in2 = 2:19 )
         i6a(in1,in2) = in1 + in2
      END FORALL
      i6a(in1,20) = in2
      FORALL ( integer(2)::in2 = 20:1:-1 )
         i9a(in1,in2) = in1 + in2
      END FORALL
      FORALL ( integer(2)::in2 = 1:20:2 )
         i10a(in1,in2) = in1
      END FORALL
      FORALL ( integer(2)::in2 = 20:1:-2 )
         i10a(in1,in2) = in2
      END FORALL
   END FORALL

   print *,i1a
   print *,i2a
   print *,i2b
   print *,i3a
   print *,i4a
   print *,i5a
   print *,i5b
   print *,i6a
   print *,i9a
   print *,i10a

END
