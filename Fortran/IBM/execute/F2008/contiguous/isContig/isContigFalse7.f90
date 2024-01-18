! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : isContigFalse7.f
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
PROGRAM isContigFalse7
      IMPLICIT NONE

      COMPLEX(16), TARGET   :: Zarr(10)
      COMPLEX(16), POINTER  :: ptr(:)
      CLASS(*), POINTER     :: upoly(:)

      COMPLEX(16), TARGET   :: Zcube(5,5,5)
      COMPLEX(16), POINTER  :: ptr3(:,:,:)
      CLASS(*), POINTER     :: upoly3(:,:,:)

!*    Data pointer assignment 

      ptr => Zarr                                                 ! contiguous
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 10

      ASSOCIATE( a => ptr(1:10:2) )                               ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)           )  ERROR STOP 11
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr(1:10:2)) )  ERROR STOP 12
      END ASSOCIATE

      ASSOCIATE( a => ptr(5:1:-1) )                               ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)           )  ERROR STOP 13
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr(5:1:-1)) )  ERROR STOP 14
      END ASSOCIATE

      ASSOCIATE( a => ptr([1,2,3,4,5]) )                          ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)                )  ERROR STOP 15
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr([1,2,3,4,5])) )  ERROR STOP 16
      END ASSOCIATE

!*    Allocatable array 

      ALLOCATE(COMPLEX(8):: upoly(10))                            ! contiguous
      IF ( .NOT. IS_CONTIGUOUS(upoly) )  ERROR STOP 20

      ASSOCIATE( a => upoly(1:10:2) )                             ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)             )  ERROR STOP 21
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly(1:10:2)) )  ERROR STOP 22
      END ASSOCIATE

      ASSOCIATE( a => upoly(5:1:-1) )                             ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)             )  ERROR STOP 23
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly(5:1:-1)) )  ERROR STOP 24
      END ASSOCIATE

      ASSOCIATE( a => upoly([1,2,3,4,5]) )                        ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)                  )  ERROR STOP 25
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly([1,2,3,4,5])) )  ERROR STOP 26
      END ASSOCIATE
!*
      DEALLOCATE(upoly) 
      ALLOCATE(upoly(10), SOURCE=Zarr)
      IF ( .NOT. IS_CONTIGUOUS(upoly) )  ERROR STOP 30

      SELECT TYPE ( s => upoly(5:1:-1) )                          ! Not contiguous
          TYPEIS (COMPLEX(16))
              IF (                         IS_CONTIGUOUS(s)             )  ERROR STOP 33
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(upoly(5:1:-1)) )  ERROR STOP 34
          CLASSDEFAULT
              ERROR STOP 35
      END SELECT

      SELECT TYPE ( s => upoly([1,2,3,4,5]) )                     ! Not contiguous
          TYPEIS (COMPLEX(16))
              IF (                         IS_CONTIGUOUS(s)                  )  ERROR STOP 37
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(upoly([1,2,3,4,5])) )  ERROR STOP 38
          CLASSDEFAULT
              ERROR STOP 39
      END SELECT

!********************** rank > 1 **************************

!*    Pointer allocation
      ptr3 => Zcube
      IF ( .NOT. IS_CONTIGUOUS(ptr3) )  ERROR STOP 40

      ASSOCIATE( a => ptr3(1,1,4:5) )     ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)              )  ERROR STOP 41
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr3(1,1,4:5)) )  ERROR STOP 42
      END ASSOCIATE

      ASSOCIATE( a => ptr3(5,:,:) )       ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)            )  ERROR STOP 43
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr3(5,:,:)) )  ERROR STOP 44
      END ASSOCIATE

      ASSOCIATE( a => ptr3(:,:,5:1:-1) )  ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)                 )  ERROR STOP 45
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr3(:,:,5:1:-1)) )  ERROR STOP 46
      END ASSOCIATE

      ASSOCIATE( a => ptr3(:,:,[1,2]) )   ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)                )  ERROR STOP 47
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(ptr3(:,:,[1,2])) )  ERROR STOP 48
      END ASSOCIATE

!*    Allocatable array
      ALLOCATE(INTEGER :: upoly3(5,5,5))
      IF ( .NOT. IS_CONTIGUOUS(upoly3) )  ERROR STOP 50

      ASSOCIATE( a => upoly3(5,:,:) )    ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)              )  ERROR STOP 51
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly3(5,:,:)) )  ERROR STOP 52 
      END ASSOCIATE

      ASSOCIATE( a => upoly3(1,1,4:5) )  ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)                )  ERROR STOP 53 
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly3(1,1,4:5)) )  ERROR STOP 54 
      END ASSOCIATE

      ASSOCIATE( a => upoly3(:,:,5:1:-1) )  ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)                   )  ERROR STOP 55 
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly3(:,:,5:1:-1)) )  ERROR STOP 56 
      END ASSOCIATE

      ASSOCIATE( a => upoly3(:,:,[1,2]) )   ! Not contiguous
        IF (                         IS_CONTIGUOUS(a)                  )  ERROR STOP 57 
        IF ( IS_CONTIGUOUS(a) .NEQV. IS_CONTIGUOUS(upoly3(:,:,[1,2])) )  ERROR STOP 58
      END ASSOCIATE
!*
      DEALLOCATE(upoly3)
      upoly3 => Zcube
      IF ( .NOT. IS_CONTIGUOUS(upoly3) )  ERROR STOP 60

      SELECT TYPE ( s => upoly3(5,:,:) )        ! Not contiguous
          TYPEIS (COMPLEX(16))
              IF (                         IS_CONTIGUOUS(s)              )  ERROR STOP 61
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(upoly3(5,:,:)) )  ERROR STOP 62
          CLASSDEFAULT
              ERROR STOP 125
      END SELECT

      SELECT TYPE ( s => upoly3(1,1,4:5) )      ! Not contiguous
          TYPEIS (COMPLEX(16))
              IF (                         IS_CONTIGUOUS(s)                )  ERROR STOP 63
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(upoly3(1,1,4:5)) )  ERROR STOP 64
          CLASSDEFAULT
              ERROR STOP 128
      END SELECT

      SELECT TYPE ( s => upoly3(:,:,1:5:-1) )   ! Not contiguous
          TYPEIS (COMPLEX(16))
              IF (                         IS_CONTIGUOUS(s)                   )  ERROR STOP 65
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(upoly3(:,:,1:5:-1)) )  ERROR STOP 66
          CLASSDEFAULT
              ERROR STOP 131
      END SELECT

      SELECT TYPE ( s => upoly3(:,:,[1,2]) )    ! Not contiguous
          TYPEIS (COMPLEX(16))
              IF (                         IS_CONTIGUOUS(s)                  )  ERROR STOP 67
              IF ( IS_CONTIGUOUS(s) .NEQV. IS_CONTIGUOUS(upoly3(:,:,[1,2])) )  ERROR STOP 68
          CLASSDEFAULT
              ERROR STOP 134
      END SELECT

END PROGRAM isContigFalse7
