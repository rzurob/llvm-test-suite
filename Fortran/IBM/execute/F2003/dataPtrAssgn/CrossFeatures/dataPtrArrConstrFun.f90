!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrArrConstrFun.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 20, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289075 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  Array construction functions 
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrArrConstrFun 
  IMPLICIT NONE

  CHARACTER, ALLOCATABLE,  TARGET  :: Tar2(:, :)
  CHARACTER, ALLOCATABLE,  TARGET  :: Tar1(:)
  CHARACTER, POINTER               :: Ptr(:, :)
  INTEGER    :: I, J, K, N
  LOGICAL                          :: L(3, 3)

  N = 3; K = 0
  ALLOCATE(Tar1(N*N))
  ALLOCATE(Tar2(N, N))

  Tar2 = RESHAPE((/"1","4","7","2","5","8","3","6","9"/), (/3,3/))
  Tar1 = (/"1","4","7","2","5","8","3","6","9"/)
  L = RESHAPE((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE./), &
              (/3,3/))

  DO I =1, N 
  DO J =I, N
  
    Ptr(I:, J:) => Tar2 

    IF (ANY( CSHIFT(Ptr,  SHIFT=-1, DIM=2)                   .NE. &
             CSHIFT(Tar2, SHIFT=-1, DIM=2)))                      STOP 31
    IF (ANY( EOSHIFT(Ptr, SHIFT=-1, BOUNDARY="*", DIM=2)     .NE. &
             EOSHIFT(Tar2,SHIFT=-1, BOUNDARY="*", DIM=2) ))       STOP 32
    IF (ANY( MERGE(Ptr,  Ptr,  L)                            .NE. &
             MERGE(Tar2, Tar2, L) ))                              STOP 33
    IF (ANY( SPREAD(Ptr,  DIM=1, NCOPIES=2)                  .NE. &
             SPREAD(Tar2, DIM=1, NCOPIES=2) ))                    STOP 34
    IF (ANY( TRANSPOSE(Ptr)                                  .NE. &
             TRANSPOSE(Tar2)))                                    STOP 35
    CALL Check2()


    Ptr(I:J, I:J) => Tar1 

    IF (ANY( RESHAPE(Ptr, (/J-I+1, J-I+1/))                       .NE. &
             RESHAPE(Tar1(1:(J-I+1)*(J-I+1)), (/J-I+1, J-I+1/))))      STOP 36
    IF (ANY( PACK(Ptr,   L)                                       .NE. &
             PACK(RESHAPE(Tar1(1:(J-I+1)*(J-I+1)-1), (/J-I+1, J-I+1/)),  L) ))  STOP 37

    CALL Check1()
 
  END DO
  END DO

  CONTAINS

  SUBROUTINE Check1()
    IF (SIZE(Ptr)  .NE. (J-I+1)*(J-I+1))            STOP 20
    IF (.NOT. ASSOCIATED(Ptr))                      STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))           STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))           STOP 23
  END SUBROUTINE 

  SUBROUTINE Check2()
    IF (SIZE(Ptr)  .NE. N*N )                    STOP 10
    IF (.NOT. ASSOCIATED(Ptr, Tar2))             STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) STOP 13
  END SUBROUTINE 

  END



