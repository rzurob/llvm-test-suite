! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : isContigTrue9.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : IS_CONTIGUOUS intrinsic
!*                             :
!*  SECONDARY FUNCTIONS TESTED : - ASSOCIATE and SELECTTYPE
!*                               - complex type
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - 
!*                               - 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
PROGRAM isContigTrue9
      IMPLICIT NONE

      COMPLEX, TARGET                :: Zarr(10)
      COMPLEX, POINTER               :: ptr(:)
      CLASS(*), ALLOCATABLE          :: upoly(:)

      COMPLEX                        :: Zcube(5,5,5)
      COMPLEX, POINTER               :: pcube(:,:,:)
      CLASS(*), ALLOCATABLE          :: up_cube(:,:,:)

!*    Data pointer assignment 

      ptr => Zarr                                                 ! contiguous
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 10

      ASSOCIATE( a => ptr )                                       ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)   )  ERROR STOP 11
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr) )  ERROR STOP 12
      END ASSOCIATE

      ASSOCIATE( a => ptr(5:) )                                   ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)       )  ERROR STOP 13
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr(5:)) )  ERROR STOP 14
      END ASSOCIATE

      ASSOCIATE( a => ptr(1:0) )                                  ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)        )  ERROR STOP 15
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr(1:0)) )  ERROR STOP 16
      END ASSOCIATE

      ASSOCIATE( a => ptr(4:9) )                                  ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)        )  ERROR STOP 17
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr(4:9)) )  ERROR STOP 18
      END ASSOCIATE

      ASSOCIATE( a => ptr(2:10:1) )                               ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)           )  ERROR STOP 19
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr(2:10:1)) )  ERROR STOP 20
      END ASSOCIATE

!*    Allocatable array 

      ALLOCATE(COMPLEX(8):: upoly(10))                            ! contiguous
      IF ( .NOT. IS_CONTIGUOUS(upoly) )  ERROR STOP 30

      ASSOCIATE( a => upoly )                                     ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)     )  ERROR STOP 31
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly) )  ERROR STOP 32
      END ASSOCIATE

      ASSOCIATE( a => upoly(5:) )                                 ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)         )  ERROR STOP 33
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly(5:)) )  ERROR STOP 34
      END ASSOCIATE

      ASSOCIATE( a => upoly(1:0) )                                ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)          )  ERROR STOP 35
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly(1:0)) )  ERROR STOP 36
      END ASSOCIATE

      ASSOCIATE( a => upoly(4:9) )                                ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)          )  ERROR STOP 37
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly(4:9)) )  ERROR STOP 38
      END ASSOCIATE

      ASSOCIATE( a => upoly(2:10:1) )                             ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)             )  ERROR STOP 39
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly(2:10:1)) )  ERROR STOP 40
      END ASSOCIATE

!*
      DEALLOCATE(upoly) 
      ALLOCATE(upoly(10), SOURCE=Zarr)
      IF ( .NOT. IS_CONTIGUOUS(upoly) )  ERROR STOP 50

      SELECT TYPE ( s => upoly )                                  ! contiguous
          TYPEIS (COMPLEX)
              IF (                  .NOT.  IS_CONTIGUOUS(s)     )  ERROR STOP 51
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(upoly) )  ERROR STOP 52
          CLASSDEFAULT
              ERROR STOP 53
      END SELECT

      SELECT TYPE ( s => upoly(:9) )                              ! contiguous
          TYPEIS (COMPLEX)
              IF (                  .NOT.  IS_CONTIGUOUS(s)         )  ERROR STOP 54
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(upoly(:9)) )  ERROR STOP 55
          CLASSDEFAULT
              ERROR STOP 56
      END SELECT

      SELECT TYPE ( s => upoly(1:0) )                             ! contiguous
          TYPEIS (COMPLEX)
              IF (                  .NOT.  IS_CONTIGUOUS(s)          )  ERROR STOP 57
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(upoly(1:0)) )  ERROR STOP 58
          CLASSDEFAULT
              ERROR STOP 59
      END SELECT

      SELECT TYPE ( s => upoly(2:10:1) )                          ! contiguous
          TYPEIS (COMPLEX)
              IF (                  .NOT.  IS_CONTIGUOUS(s)             )  ERROR STOP 60
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(upoly(2:10:1)) )  ERROR STOP 61
          CLASSDEFAULT
              ERROR STOP 62
      END SELECT

!********************** rank > 1 **************************

!*    Pointer allocation
      ALLOCATE(pcube(5,5,5))
      IF ( .NOT. IS_CONTIGUOUS(pcube) )  ERROR STOP 70

      ASSOCIATE( a => pcube )              ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)     )  ERROR STOP 71
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(pcube) )  ERROR STOP 72
      END ASSOCIATE

      ASSOCIATE( a => pcube(1:5,:,:) )     ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)              )  ERROR STOP 73
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(pcube(1:5,:,:)) )  ERROR STOP 74
      END ASSOCIATE

      ASSOCIATE( a => pcube(:,:,1:5:1) )   ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)                )  ERROR STOP 75
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(pcube(:,:,1:5:1)) )  ERROR STOP 76
      END ASSOCIATE

      ASSOCIATE( a => pcube(:,:,5) )       ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)            )  ERROR STOP 77
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(pcube(:,:,5)) )  ERROR STOP 78
      END ASSOCIATE

!*    Allocatable array
      ALLOCATE(COMPLEX(8):: up_cube(5,5,5))
      IF ( .NOT. IS_CONTIGUOUS(up_cube) )  ERROR STOP 80

      ASSOCIATE( a => up_cube )            ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)       )  ERROR STOP 81
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(up_cube) )  ERROR STOP 82
      END ASSOCIATE

      ASSOCIATE( a => up_cube(1:5,:,:) )   ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)                )  ERROR STOP 83
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(up_cube(1:5,:,:)) )  ERROR STOP 84
      END ASSOCIATE

      ASSOCIATE( a => up_cube(:,:,1:5:1) )   ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)                  )  ERROR STOP 85
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(up_cube(:,:,1:5:1)) )  ERROR STOP 86
      END ASSOCIATE

      ASSOCIATE( a => up_cube(:,:,5) )       ! contiguous
        IF (                  .NOT.  IS_CONTIGUOUS(a)              )  ERROR STOP 87
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(up_cube(:,:,5)) )  ERROR STOP 88
      END ASSOCIATE

!*
      DEALLOCATE(up_cube)
      ALLOCATE(up_cube(5,5,5), SOURCE=Zcube)
      IF ( .NOT. IS_CONTIGUOUS(up_cube) )  ERROR STOP 90

      SELECT TYPE ( s => up_cube )               ! contiguous
          TYPEIS (COMPLEX)
              IF (                  .NOT.  IS_CONTIGUOUS(s)       )  ERROR STOP 91
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(up_cube) )  ERROR STOP 92
          CLASSDEFAULT
              ERROR STOP 93
      END SELECT

      SELECT TYPE ( s => up_cube(1:5,:,:) )      ! contiguous
          TYPEIS (COMPLEX)
              IF (                  .NOT.  IS_CONTIGUOUS(s)                )  ERROR STOP 94
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(up_cube(1:5,:,:)) )  ERROR STOP 95
          CLASSDEFAULT
              ERROR STOP 96
      END SELECT

      SELECT TYPE ( s => up_cube(:,:,1:5:1) )    ! contiguous
          TYPEIS (COMPLEX)
              IF (                  .NOT.  IS_CONTIGUOUS(s)                  )  ERROR STOP 97
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(up_cube(:,:,1:5:1)) )  ERROR STOP 98
          CLASSDEFAULT
              ERROR STOP 99
      END SELECT

      SELECT TYPE ( s => up_cube(:,:,5) )        ! contiguous
          TYPEIS (COMPLEX)
              IF (                  .NOT.  IS_CONTIGUOUS(s)              )  ERROR STOP 100
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(up_cube(:,:,5)) )  ERROR STOP 101
          CLASSDEFAULT
              ERROR STOP 102
      END SELECT
END PROGRAM isContigTrue9
