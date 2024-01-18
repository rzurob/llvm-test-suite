
      INTEGER, TARGET               :: I5D(1,1,1,1,2)
      INTEGER, POINTER              :: ptr5D(:,:,:,:,:)

      ptr5D => I5D(:,:,:,:,1:2)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 44
      IF ( .NOT. IS_CONTIGUOUS(ptr5D(1,1,1,1,1:2)) )  ERROR STOP 58

END
