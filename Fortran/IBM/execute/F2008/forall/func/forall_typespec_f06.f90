!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : forall_typespec_f06.f
!*
!*  DATE                       : 2012-06-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : FORALL with type specifier (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED : Compatibility with !HPF$ align and distribute
!*                               tags
!*  ADAPTED FROM               : hpf_forall/construct/fxfc006.scenario
!*
!*  DESCRIPTION
!*
!*    Test simple Forall construct with a scalar-mask-expr.  Test forall
!*    assignment as the forall-body-stmt.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM fxfc007

   !HPF$ PROCESSORS P(4)

   integer CASENUM
   integer in1,in2

   !--------------------------------
   ! Variable Declarations
   !--------------------------------

   logical,parameter,dimension(100) :: l5a_nc = (/((/.TRUE.,.FALSE./),i=1,50)/)
   !HPF$ DISTRIBUTE l5a_nc(BLOCK) ONTO P

   integer i1a(100)
   !HPF$ DISTRIBUTE i1a(BLOCK) ONTO P

   integer i2a(100)
   !HPF$ DISTRIBUTE i2a(BLOCK(30)) ONTO P

   integer i3a(100)
   !HPF$ DISTRIBUTE i3a(CYCLIC) ONTO P

   integer i4a(100)
   !HPF$ DISTRIBUTE i4a(CYCLIC(5)) ONTO P

   integer i5a(100)
   !HPF$ ALIGN i5a(:) WITH l5a_nc(:)

   integer i6a(100)
   logical l6a(100)

   !--------------------------------
   ! End of Variable Declarations
   !--------------------------------

   !-------------------------------------------------
   ! Mask is .TRUE.
   !-------------------------------------------------
   CASENUM = 1
   PRINT *,CASENUM

   i1a = 0
   FORALL ( integer(1) :: in1 = 1:100 , .TRUE. )
      i1a(in1) = in1
   END FORALL

   PRINT *,i1a

   !-------------------------------------------------
   ! Mask is .FALSE.
   !-------------------------------------------------
   CASENUM = 2
   PRINT *,CASENUM

   i2a = 0
   FORALL ( integer(1)::in1 = 1:100, .FALSE. )
      i2a(in1) = in1
   END FORALL

   PRINT *,i2a

   !-------------------------------------------------
   ! Mask is all TRUE
   !-------------------------------------------------
   CASENUM = 3
   PRINT *,CASENUM

   i3a = 0
   FORALL ( integer(1)::in1 = 1:99 , in1 > i3a(in1) )
      i3a(in1+1) = in1 + 2
   END FORALL

   PRINT *,i3a

   !-------------------------------------------------
   ! Mask is all FALSE
   !-------------------------------------------------
   CASENUM = 4
   PRINT *,CASENUM

   i4a = 0
   FORALL ( integer(1)::in1 = 1:99, i4a(in1) > in1 )
      i4a(in1) = in1
   END FORALL

   PRINT *,i4a

   !-------------------------------------------------
   ! Mask is logical named constant
   !-------------------------------------------------
   CASENUM = 5
   PRINT *,CASENUM

   i5a = 0
   FORALL ( integer(1):: in1 = 1:100, l5a_nc(in1) )
      i5a(in1) = in1
   END FORALL

   PRINT *,i5a

   !-------------------------------------------------
   ! Mask is logical expression
   !-------------------------------------------------
   CASENUM = 6
   PRINT *,CASENUM

   i6a = 0
   FORALL ( integer(1)::in1 = 1:100 , in1/2 .NE. (in1+1)/2 )
      l6a(in1) = .TRUE.
      l6a(in1+1) = .FALSE.
   END FORALL
   FORALL ( in1 = 1:100, l6a(in1) )
      i6a(in1) = in1
   END FORALL

   PRINT *,l6a
   PRINT *,i6a


END
