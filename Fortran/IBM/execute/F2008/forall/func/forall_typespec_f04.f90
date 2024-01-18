!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_f04.f
!*
!*  DATE                       : 2012-06-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : Compatibility with !HPF$ Processor tag
!*  ADAPTED FROM               : hpf_forall/construct/fxfc009.scenario
!*
!*  DESCRIPTION
!*
!*    Test simple Forall construct with a scalar-mask-expr.  Test forall pointer
!*    assignment in forall-stmt-body.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM fxfc009

   !HPF$ PROCESSORS P(4)

   integer CASENUM
   integer in1,in2

   !--------------------------------
   ! Variable Declarations
   !--------------------------------

   INTEGER,TARGET :: i1_t(20,20)
   INTEGER,POINTER :: i1_p(:,:)

   INTEGER,TARGET :: i2_t(100)
   INTEGER,POINTER :: i2_p(:)

   INTEGER,TARGET :: i3_t(20,20)
   INTEGER,POINTER :: i3_p(:,:)

   REAL,TARGET :: r4_t(100)
   REAL,POINTER :: r4_p(:)

   INTEGER,TARGET :: i5_t(100)
   INTEGER,POINTER :: i5_p(:)

   TYPE dt
      SEQUENCE
      integer,pointer :: p
   END TYPE dt
   TYPE (dt) :: dt_6(100)

   INTEGER,TARGET :: i6a(100)

   !--------------------------------
   ! End of Variable Declarations
   !--------------------------------

   !-------------------------------------------------
   ! Scalar constant in mask
   !-------------------------------------------------
   CASENUM = 1
   PRINT *,CASENUM

   i1_t = 0
   i1_p => i1_t

   FORALL ( integer(4)::in1 = 1:20 )
      FORALL ( integer*4::in2 = 1:20, .TRUE. )
         i1_p(in1,in2) = in1 + in2
      END FORALL
   END FORALL

   PRINT *,i1_t

   !-------------------------------------------------
   ! scalar variable
   !-------------------------------------------------
   CASENUM = 2
   PRINT *,CASENUM

   i2_t = 0
   i2_p => i2_t

   FORALL ( integer*2::in1 = 1:100 )
      i2_p(in1) = in1
   END FORALL

   PRINT *,i2_t

   !-------------------------------------------------
   ! Simple Expression
   !-------------------------------------------------
   CASENUM = 3
   PRINT *,CASENUM

   i3_t = 0
   i3_p => i3_t

   FORALL ( integer*4::in1 = 1:20 )
      FORALL ( integer(4)::in2 = 1:20 , in1 > in2 )
         i3_p(in1,in2) = in1 + in2
      END FORALL
   END FORALL

   PRINT *,i3_t

   !-------------------------------------------------
   ! Array index
   !-------------------------------------------------
   CASENUM = 4
   PRINT *,CASENUM

   r4_t = 0.0
   r4_p => r4_t

   forall ( integer*1::in1 = 1:100 ) r4_p(in1) = real(in1) / 2
   FORALL ( integer*2::in1 = 1:100 , r4_t(in1) .EQ. real(in1/2) )
      r4_p(in1) = 99
   END FORALL

   PRINT *,r4_t

   !-------------------------------------------------
   ! Intrinsic Function
   !-------------------------------------------------
   CASENUM = 5
   PRINT *,CASENUM

   i5_t = 0
   i5_p => i5_t

   forall ( integer(1)::in1 = 1:100 ) i5_p(in1) = 101 - in1
   FORALL ( integer(2)::in1 = 1:99, SUM(i5_p(in1:)) > 5000 )
      i5_p(in1) = 999
   END FORALL

   PRINT *,i5_t

END
