! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/OO_poly/selectType/Quotes/TypeParam1.f
! opt variations: -qnock -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  TypeParam1.f
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:  
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : TypeParam1
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 27, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     
!*  the length type parameter 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE :: DT(K1,N1)    ! (1,4)
      INTEGER, KIND                           :: K1
      INTEGER, LEN                            :: N1
      CHARACTER(kind=K1,len=N1), ALLOCATABLE   :: CArr(:) 
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetStr
    END TYPE

  CONTAINS

    FUNCTION GetStr(Num, Obj)
    CLASS(DT(1,*)), INTENT(IN)    :: Obj 
    INTEGER, INTENT(IN)      :: Num
    CHARACTER(LEN(Obj%CArr)) :: GetStr 
      GetStr = Obj%CArr(Num) 
    END FUNCTION

  END MODULE


  PROGRAM TypeParam1 
  USE M
  IMPLICIT NONE

  TYPE(DT(1,4)), TARGET   ::  DTV(3,3,3)
  CLASS(DT(1,:)), POINTER :: Ptr(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J

  ! Ptr => Dtv 
    ALLOCATE(Ptr(2,2,2), SOURCE=DT(1,4)(CArr=(/"1234","4321"/)))

    SELECT TYPE (U => Ptr(S,S,S))
2     CLASS DEFAULT
        SELECT TYPE (U => U)
          CLASS IS (DT(1,*))
            IF (SIZE(U(2,2,2)%CArr) .NE. 2)      STOP 30
            IF (U(2,1,2)%CArr(1   ) .NE. "1234") STOP 31
            IF (U(1,1,2)%CArr(2)    .NE. "4321") STOP 32
            IF (LEN(U(2,2,2)%CArr)  .NE. 4)      STOP 33
          CLASS DEFAULT
            STOP 34
        END SELECT
    END SELECT
  END


