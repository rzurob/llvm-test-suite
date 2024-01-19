! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/allocEnh/basic/nonchar/unallocatedLBoundDerived04.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*                               Non-CHARACTER Derived Type
!*
!*  DATE                       : September  6, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for an N-Dimension
!*                               Array (1<= N <= 7) -- N == 6
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : TYPE, ALLOCATABLE Attribute, Intrinsic
!*                               Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
!*
!*  7.4.1.3 Interpretation of intrinsic assignments
!*
!*  If variable is an allocated allocatable variable, it is deallocated if
!*  expr is an array of different shape or any of the corresponding length
!*  type parameter values of variable and expr differ. If variable is or
!*  becomes an unallocated allocatable variable, then it is allocated with
!*  each deferred type parameter equal to the corresponding type parameters
!*  of expr, with the shape of expr, and with each lower bound equal to the
!*  corresponding element of LBOUND(expr).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM unallocatedLBoundDerived04

    TYPE tB(N1,K1)    ! (20,4)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        REAL(K1)      :: bR
    END TYPE tB

    TYPE, EXTENDS(tB) :: tD    ! (20,4)
        INTEGER(K1) :: dI
    END TYPE tD


    INTEGER(4) :: i

    INTEGER, PARAMETER :: idx( 0:11 ) = (/ -4,-2, -2,0, -1,1, 0,2, 2,4, 3,5 /)

    TYPE(tD(20,4)) :: d( idx(  0 ):idx(  1 ),&
                   idx(  2 ):idx(  3 ),&
                   idx(  4 ):idx(  5 ),&
                   idx(  6 ):idx(  7 ),&
                   idx(  8 ):idx(  9 ),&
                   idx( 10 ):idx( 11 ) ) =&
        RESHAPE((/ (tD(20,4)(REAL( i ), i), i = 1, 3**6) /), (/ 3,3,3,3,3,3 /))

    TYPE(tD(:,4)), ALLOCATABLE :: dA( :,:,:,:,:,: )


    IF ( ALLOCATED( dA ) ) CALL zzrc( 10_4 )


    dA = d


    IF (.NOT. ALLOCATED( dA ))  CALL zzrc( 20_4 )


    PRINT *, SIZE( dA )
    IF (SIZE( dA ) /= 3**6)     CALL zzrc( 30_4 )

    DO i = 0_4, 5_4
        PRINT *, i, SIZE(dA, (i + 1_4)), LBOUND(dA, (i + 1_4))

        IF (SIZE(dA, (i + 1_4)) /= 3)                   CALL zzrc( (31_4 + i) )
        IF (LBOUND(dA, (i + 1_4)) /= idx( (i * 2_4) ))  CALL zzrc( (41_4 + i) )
    END DO


    IF (.NOT. ALL( (dA%bR == d%bR) )) THEN
        PRINT 10, dA%bR
10      FORMAT('dA%bR = (',728(F6.2,','),F6.2,')')

        CALL zzrc( 50_4 )

    ELSE IF (.NOT. ALL( (dA%dI == d%dI) )) THEN
        PRINT 20, dA%dI
20      FORMAT('dA%dI = (',728(I3,','),I3,')')

        CALL zzrc( 60_4 )
    END IF

END PROGRAM unallocatedLBoundDerived04
