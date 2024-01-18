!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : d327313 - expr is a Scalar and variable is
!*                               an Array
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 27, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2003: ALLOC: Run-Time SIGSEGV with Derived
!*                               Type with ALLOCATABLE Component in IF/ELSE IF
!*                               Construct
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  The Reduced Code below defines an instance of an Array of Derived Type
!*  (with an ALLOCATABLE Component), and assigns a value to the Component.
!*
!*  This Test Case coredumps with a Memory fault in the ELSE IF of the
!*  IF/ELSE IF Construct where the Array Element uses a variable as the
!*  Subscript.  The coredump doesn't occur if the variable is replaced
!*  with a literal value.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM allocArrVarScalarExpr03

    TYPE :: tType2
        INTEGER, ALLOCATABLE :: t1
    END TYPE tType2

    INTEGER :: i = 1
    TYPE(tType2), ALLOCATABLE :: t2( : )

    ALLOCATE( t2( 1 ) )
    t2( 1 )%t1 = 2

    IF ( .FALSE. ) THEN
        CALL zzrc( 20_4 )
    ELSE IF (t2( i )%t1 /= 2) THEN
        CALL zzrc( 30_4 )
    END IF

END PROGRAM allocArrVarScalarExpr03
