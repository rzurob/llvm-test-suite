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
!*  PRIMARY FUNCTIONS TESTED   : pointer array, VOLATILE
!*
!*
!*  DESCRIPTION                : diagnostic TC for  C1233
!*
!*  C1233 (R1221) If an actual argument is a pointer array, and the
!*        corresponding dummy argument has either the VOLATILE or ASYNCHRONOUS
!*        attribute, that dummy argument shall be an assumed-shape array or
!*        a pointer array.
!* ===================================================================

  program volatileC1233PointerArray02d 

    interface
        subroutine ExplicitArray(x)
            integer, VOLATILE :: x(*)
        end subroutine
    end interface

    integer, pointer :: ptrArray(:,:)
    integer, target  :: tarAttay(5,9)

    ptrArray => tarAttay

    call ExplicitArray(ptrArray)

  end program volatileC1233PointerArray02d 

  subroutine ExplicitArray(x)
       integer, VOLATILE :: x(*)     ! dummy argument is assumed size array
  end subroutine ExplicitArray


