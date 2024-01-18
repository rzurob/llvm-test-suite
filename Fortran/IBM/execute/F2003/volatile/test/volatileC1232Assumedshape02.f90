!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : assumed-shape array, VOLATILE
!*
!*  DESCRIPTION                : functional TC for  C1232
!*
!*  C1232 (R1221) If an actual argument is an array section or an
!*                assumed-shape array, and the corresponding dummy argument
!*                has either the VOLATILE or ASYNCHRONOUS attribute, that
!*                dummy argument shall be an assumed-shape array.
!* ===================================================================

  program volatileC1232Assumedshape02

    interface
       subroutine innerAssumeShapeVolatile(x)
         integer x(1:,:,:)
       end subroutine innerAssumeShapeVolatile
    end interface

    integer y(5,6,10)
    call innerAssumeShapeVolatile(y)

  end program volatileC1232Assumedshape02

  subroutine innerAssumeShapeVolatile(x)

    interface
      subroutine externalAssumeShapeVolatile(a)
           integer, VOLATILE:: a(:,:,:)
      end subroutine externalAssumeShapeVolatile
    end interface

    integer x(1:, :, :)
    call externalAssumeShapeVolatile(x)

  end subroutine innerAssumeShapeVolatile

  subroutine externalAssumeShapeVolatile(a)
       integer, VOLATILE:: a(:,:,:)             !dummy arg. is assumed-shape
  end subroutine externalAssumeShapeVolatile    ! array
