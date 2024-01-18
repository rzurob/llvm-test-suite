!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 30/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : assumed-shape array, VOLATILE
!*
!*
!*  DESCRIPTION                : diagnostic TC for  C1232
!*
!*  C1232 (R1221) If an actual argument is an array section or an
!*                assumed-shape array, and the corresponding dummy argument
!*                has either the VOLATILE or ASYNCHRONOUS attribute, that
!*                dummy argument shall be an assumed-shape array.
!* ===================================================================

  program volatileC1232Assumedshape01d 

    interface
       subroutine innerAssumeShapeVolatile(x)
         integer x(1:,:,:)
       end subroutine innerAssumeShapeVolatile
    end interface

    integer y(5,6,10)
    call innerAssumeShapeVolatile(y)

  end program volatileC1232Assumedshape01d

  subroutine innerAssumeShapeVolatile(x)

    interface
      subroutine externalAssumeShapeVolatile(a)
           integer, VOLATILE:: a(5,6,10)
      end subroutine externalAssumeShapeVolatile
    end interface  

    integer x(1:, :, :)                     ! actual argument is assumed 
    call externalAssumeShapeVolatile(x)     ! shape array
    
  end subroutine innerAssumeShapeVolatile

  subroutine externalAssumeShapeVolatile(a)
       integer, VOLATILE:: a(5,6,10)         ! dummy arg. is explicit array
  end subroutine externalAssumeShapeVolatile
